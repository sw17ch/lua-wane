{-# LANGUAGE FlexibleInstances, CPP #-}

-- | Shorten all the locally defined names in a chunk (local
-- variables/functions, function arguments, for-loop variables).
--
-- The general strategy is to traverse the term and substitute names
-- with newly generated short-names.  As we go, we accumulate a map
-- of the current substitutions in scope.
--
-- We avoid variable capture by always generating a fresh short-name
-- for each local variable: this way we don't clash with another local
-- variable in an inner or outer scope.
--
-- We avoid clashes with global variables by treating them as fully
-- qualified; for example, we interpret "print" as "_ENV.print", and
-- then treat "_ENV" as any other name.  We do this to simplify the
-- algorithm (otherwise we would have to check that the generated
-- names don't clash with global variables).
--
-- Note: "fresh" here means "fresh w.r.t. the substitutions in scope";
-- once we pop out of a scope, we reuse those names.  This way we
-- don't needlessly chew through short-names.
--
-- We define this as a special-purpose algorithm (instead of writing a
-- general-purpose implementation of capture-avoiding substitution)
-- for efficiency reasons: this way we can produce the result term in
-- one traversal of the input term.
--

module Language.Lua.ShortenNames
    ( shortenNames
    , test
    , testFile
    )
    where

import           Language.Lua.Syntax
import qualified Data.Map.Strict as Map
import           Data.List (foldl')

import qualified Language.Lua.Parser as P
import qualified Language.Lua.PrettyPrinter as PP


----------------------------------------------------------------
-- Nicknames and environments
----------------------------------------------------------------

-- A nickname is just a short name; type alias for clarity
--
type Nick = Name

data Env =
  Env { substitutions :: Map.Map Name Nick  -- the current substitutions in play
      , currNick      :: Nick               -- the latest nickname we have generated
      }

-- | Lookup the nickname for a name.
--
-- Any name that does not have a nickname must be a reference to a
-- global variable (since we generate a new nickname for every local
-- variable that is introduced).
--
-- In this case, we return the nickname for "_ENV" (or just "_ENV" if
-- there is no nickname), so that the caller can choose what to do
-- (for example, rewrite a global "foo" as "_.foo", where "_" is the
-- nickname for "_ENV").
--
lookupNick :: Env -> Name -> Either Nick Nick
lookupNick env name =
  case Map.lookup name (substitutions env) of
    Just nick -> Right nick
    Nothing   -> Left  _ENV
      where _ENV = maybe "_ENV" id (Map.lookup "_ENV" (substitutions env))

-- | Generate a new nickname, adding it to the set of current
-- substitutions.
--
genNick :: Env -> Name -> (Env, Nick)
genNick env name =
  case Map.lookup name (substitutions env) of

    -- If `name` already has a nickname, we can reuse it instead of
    -- generating a new one since we only call this function in places
    -- where `name` would be shadowing an earlier occurrence of the
    -- same name anyways.
    --
    -- This is an optional optimization (we could just collapse it
    -- with the `Nothing` case).
    --
    Just nick -> (env, nick)

    Nothing -> (env', nick)
      where nick = incrNick (currNick env)
            env' = Env { substitutions = Map.insert name nick (substitutions env)
                       , currNick      = nick
                       }

genNicks :: Env -> [Name] -> (Env, [Nick])
genNicks env0 names = fmap reverse $ foldl' go (env0, []) names
  where
    go (env, nicks) name = fmap (:nicks) $ genNick env name

-- | Increment a nickname.
--
-- Applying this repeatedly starting with "_" yields:
--
--    "_", "a", ..., "z", "A", ..., "Z", "aa", "ab", ...
--
-- We (arbitrarily) have chosen to treat "_" specially as the first
-- item in this sequence (elsewhere we set it as the nickname for "_ENV").
--
-- Of course, we could do more with numbers and underscores.
--
incrNick :: String -> String
incrNick nick =
    if next `elem` reservedNames then
      incrNick next
    else
      next

  where
    reservedNames = ["self"]

    -- We increment like a decimal numeral, so it's easier if we start
    -- with the "one's place".
    --
    next = reverse . go . reverse $ nick

    go "_"    = "a"   -- special case
    go []     = "a"
    go (c:cs) =
      case incrLetter c of
        Just c' ->  c' : cs
        Nothing -> 'a' : go cs

-- Only call this with a-z and A-Z
incrLetter :: Char -> Maybe Char
incrLetter 'z' = Just 'A'
incrLetter 'Z' = Nothing
incrLetter  x  = Just (succ x)


----------------------------------------------------------------
-- Zap class
----------------------------------------------------------------

-- To "zap" something means to make all the substitutions given by
-- Env, as well as introduce new substitutions along the way for any
-- new locally defined variables.

class Zap a where
  zap :: Env -> a -> a

instance (Zap a, Zap b) => Zap (a,b) where  -- used for If
  zap env (a,b) = (zap env a, zap env b)

-- A function (not an instance) for lists -- to force us to really
-- think about what we want to do when processing a list of things.
--
zaps :: Zap a => Env -> [a] -> [a]
zaps = map . zap

-- | Shorten all names local to this chunk.
shortenNames :: Block -> Block
shortenNames block =
    Block (assign_ENV : stats) mexps
  where
    Block stats mexps = zap initialEnv block
    initialEnv = Env { substitutions = subs, currNick = nick }
    assign_ENV = LocalAssign [nick] (Just [PrefixExp $ PEVar $ VarName "_ENV"])
    nick = "_"
    subs =
      Map.insert "_ENV" nick $
      Map.insert "self" "self" $
      Map.empty


-- Throwaway test functions

printResult result =
  putStrLn $
    either show (show . PP.pprint) (shortenNames <$> result)

test chunk =
  printResult $ P.parseText P.chunk chunk

testFile file =
  printResult =<< P.parseFile file


----------------------------------------------------------------
-- Easy cases: just recurse
----------------------------------------------------------------

instance Zap FunCall where
  zap env (NormalFunCall prefixExp funArg) =
    NormalFunCall (zap env prefixExp) (zap env funArg)

  zap env (MethodCall prefixExp name funArg) =
    MethodCall (zap env prefixExp) name (zap env funArg)

instance Zap FunArg where
  zap env (Args exps)            = Args (zaps env exps)
  zap env (TableArg tableFields) = TableArg (zaps env tableFields)
  zap env x@(StringArg{})        = x

instance Zap PrefixExp where
  zap env (PEVar var)            = PEVar (zap env var)
  zap env (PEFunCall funCall)    = PEFunCall (zap env funCall)
  zap env (Paren exp)            = Paren (zap env exp)

instance Zap TableField where
  zap env (ExpField exp1 exp2)   = ExpField (zap env exp1) (zap env exp2)
  zap env (NamedField name exp)  = NamedField name (zap env exp)
  zap env (Field exp)            = Field (zap env exp)

instance Zap Exp where
  zap env (EFunDef funBody)        = EFunDef (zap env funBody)
  zap env (PrefixExp prefixExp)    = PrefixExp (zap env prefixExp)
  zap env (TableConst tableFields) = TableConst (zaps env tableFields)
  zap env (Binop binop exp1 exp2)  = Binop binop (zap env exp1) (zap env exp2)
  zap env (Unop unop exp)          = Unop unop (zap env exp)
  zap env x                        = x


----------------------------------------------------------------
-- Medium cases: change a name to a nickname
----------------------------------------------------------------

instance Zap FunName where
  zap env (FunName name names mname) =
    case lookupNick env name of
      Right nick -> FunName nick names mname
      Left  _ENV ->
        -- global variable, e.g. "function foo.bar()" -> "function _.foo.bar()"
        FunName _ENV (name:names) mname

instance Zap Var where
  zap env (Select prefixExp exp) = Select (zap env prefixExp) (zap env exp)
  zap env (SelectName prefixExp name) = SelectName (zap env prefixExp) name
  zap env (VarName name) =
    case lookupNick env name of
      Right nick -> VarName nick
      Left  _ENV ->
        -- global variable, e.g. "print" -> "_.print"
        SelectName (PEVar $ VarName _ENV) name


----------------------------------------------------------------
-- Hard cases: generate nicknames
----------------------------------------------------------------

instance Zap FunBody where
  zap env (FunBody names bool block) =
    FunBody nicks bool (zap newEnv block)
    where
      (newEnv, nicks) = genNicks env names

-- We address `Stat`s directly in the instance for `Block` since
-- statements can affect the `Env` for later statements in the same
-- block, and it's just easier to recurse this way.
--
instance Zap Block where
  zap env (Block [] mexps) = Block [] (zaps env <$> mexps)

  zap env (Block (stat:stats) mexps) =
    case stat of

      ---- Easy sub-cases: just recurse ----

      Assign vars exps ->
        Block (stat':stats') mexps'
        where
          stat' = Assign (zaps env vars) (zaps env exps)
          Block stats' mexps' = zap env (Block stats mexps)

      FunCall funCall ->
        Block (stat':stats') mexps'
        where
          stat' = FunCall (zap env funCall)
          Block stats' mexps' = zap env (Block stats mexps)

      Do block ->
        Block (stat':stats') mexps'
        where
          stat' = Do (zap env block)
          Block stats' mexps' = zap env (Block stats mexps)

      While exp block ->
        Block (stat':stats') mexps'
        where
          stat' = While (zap env exp) (zap env block)
          Block stats' mexps' = zap env (Block stats mexps)

      Repeat block exp ->
        Block (stat':stats') mexps'
        where
          stat' = Repeat (zap env block) (zap env exp)
          Block stats' mexps' = zap env (Block stats mexps)

      If expBlocks mblock ->
        Block (stat':stats') mexps'
        where
          stat' = If (zaps env expBlocks) (zap env <$> mblock)
          Block stats' mexps' = zap env (Block stats mexps)

      FunAssign funName funBody ->
        Block (stat':stats') mexps'
        where
          stat' = FunAssign (zap env funName) (zap env funBody)
          Block stats' mexps' = zap env (Block stats mexps)


      ---- Hard sub-cases: manage names ----

      LocalAssign names local_mexps ->
        Block (stat':stats') mexps'
        where
          -- Recurse into `local_mexps` with the old env because the
          -- names of this local assignment aren't in scope there.
          --
          stat' = LocalAssign nicks (zaps env <$> local_mexps)

          -- Recurse into the rest of the block with the new env,
          -- because the names are in scope there.
          --
          Block stats' mexps' = zap newEnv (Block stats mexps)

          (newEnv, nicks) = genNicks env names

      LocalFunAssign name funBody ->
        Block (stat':stats') mexps'
        where
          -- Recurse into `funBody` with the *new* env because the
          -- name *is* in scope there (this scoping rule exists so
          -- that local function definitions can be recursive).
          --
          stat' = LocalFunAssign nick (zap newEnv funBody)
          Block stats' mexps' = zap newEnv (Block stats mexps)
          (newEnv, nick) = genNick env name

      ForIn names exps block ->
        Block (stat':stats') mexps'
        where
          -- The variables introduced by the ForIn are only in scope
          -- in the loop body, so that is the only place where we
          -- recurse with the new env.
          --
          stat' = ForIn nicks (zaps env exps) (zap newEnv block)
          Block stats' mexps' = zap env (Block stats mexps)
          (newEnv, nicks) = genNicks env names

      ForRange name exp1 exp2 mexp block ->
        Block (stat':stats') mexps'
        where
          -- Recurse with newEnv only in loop body, as with ForIn
          --
          stat' = ForRange nick (zap env exp1) (zap env exp2) (zap env <$> mexp)
                    (zap newEnv block)
          Block stats' mexps' = zap env (Block stats mexps)
          (newEnv, nick) = genNick env name


      ---- Remaining sub-cases do not contain variables: just recurse ----

      stat' ->
        Block (stat':stats') mexps'
        where
          Block stats' mexps' = zap env (Block stats mexps)
