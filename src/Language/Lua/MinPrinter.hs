{-# LANGUAGE FlexibleInstances, CPP #-}

{- This file is a derivative of the similarly named "PrettyPrinter.hs"
 - file found in language-lua on which this package depends.
 -
 - https://git.io/vwNTp -}

-- | Lua min-printer.
module Language.Lua.MinPrinter
  ( minprint
  , renderPretty
  , displayS
  , displayIO
  , LMin
  ) where

#if __GLASGOW_HASKELL__ >= 710
import           Prelude                 hiding (EQ, GT, LT, (<$>))
#else
import           Prelude                 hiding (EQ, GT, LT)
#endif

import           Text.PrettyPrint.Leijen

import           Language.Lua.Syntax
import qualified Data.List as List

cat' :: [Doc] -> Doc
cat' = foldl (<>) empty

sep' :: [Doc] -> Doc
sep' elems = cat' $ List.intersperse space elems

intercalate :: Doc -> [Doc] -> Doc
intercalate s elems = cat' (List.intersperse s elems)


type Precedence = Int

class LMin a where
    minprint :: a -> Doc
    minprint = minprint' 0

    minprint' :: Precedence -> a -> Doc
    minprint' _ = minprint

instance LMin [Char] where
    minprint = text

instance LMin Bool where
    minprint True  = text "true"
    minprint False = text "false"

instance LMin Exp where
    minprint' _ Nil            = text "nil"
    minprint' _ (Bool s)       = minprint s
    minprint' _ (Number n)     = text n
    minprint' _ (String s)     = text s
    minprint' _ Vararg         = text "..."
    minprint' _ (EFunDef f)    = minprint f
    minprint' _ (PrefixExp pe) = minprint pe
    minprint' _ (TableConst t) = minprint t
    minprint' p (Binop op e1 e2) = ps (minprint' opPrecL e1 <+> minprint op
                                                            <+> case e2 of
                                                                  Unop{} -> minprint e2
                                                                  _ -> minprint' opPrecR e2)
      where
        (opPrecL, opPrecR) = getBinopPrec op
        ps = if min opPrecL opPrecR < p then parens else id

    -- We handle this as a special case: When we have a chain of negations, we
    -- should put a space between operators, otherwise we end up printing a
    -- comment.
    --
    -- One another solution would be to always put a space after negation, but I
    -- like to put negation just before the expression, without any spaces.
    minprint' p (Unop Neg (Unop Neg e)) =
        ps (minprint Neg <+> minprint' opPrec (Unop Neg e))
      where
        opPrec = getUnopPrec Neg
        ps = if opPrec < p then parens else id

    minprint' p (Unop op e)    = ps (minprint op <> minprint' opPrec e)
      where
        opPrec = getUnopPrec op
        ps = if opPrec < p then parens else id

instance LMin Var where
    minprint (VarName n)          = minprint n
    minprint (Select pe e)        = minprint pe <> align (brackets (minprint e))
    minprint (SelectName pe name) = minprint pe <> (char '.' <> minprint name)

instance LMin Binop where
    minprint Add    = char '+'
    minprint Sub    = char '-'
    minprint Mul    = char '*'
    minprint Div    = char '/'
    minprint IDiv   = text "//"
    minprint Exp    = char '^'
    minprint Mod    = char '%'
    minprint Concat = text ".."
    minprint LT     = char '<'
    minprint LTE    = text "<="
    minprint GT     = char '>'
    minprint GTE    = text ">="
    minprint EQ     = text "=="
    minprint NEQ    = text "~="
    minprint And    = text "and"
    minprint Or     = text "or"
    minprint BAnd   = char '&'
    minprint BOr    = char '|'
    minprint BXor   = char '~'
    minprint ShiftL = text "<<"
    minprint ShiftR = text ">>"

instance LMin Unop where
    minprint Neg = char '-'
    minprint Not = text "not "
    minprint Len = char '#'
    minprint Complement = char '~'

getBinopPrec :: Binop -> (Precedence, Precedence)
getBinopPrec op =
    case op of
      Add -> (10, 10)
      Sub -> (10, 10)
      Mul -> (11, 11)
      Div -> (11, 11)
      IDiv -> (11, 11)
      Exp -> (14, 13)
      Mod -> (11, 11)
      Concat -> (9, 8)
      ShiftL -> (7, 7)
      ShiftR -> (7, 7)
      BAnd -> (6, 6)
      BXor -> (5, 5)
      BOr -> (4, 4)
      LT -> (3, 3)
      LTE -> (3, 3)
      GT -> (3, 3)
      GTE -> (3, 3)
      EQ -> (3, 3)
      NEQ -> (3, 3)
      And -> (2, 2)
      Or -> (1, 1)

getUnopPrec :: Unop -> Precedence
getUnopPrec = const 12

instance LMin PrefixExp where
    minprint (PEVar var)         = minprint var
    minprint (PEFunCall funcall) = minprint funcall
    minprint (Paren e)           = parens (minprint e)

instance LMin [TableField] where
    minprint fields = braces (intercalate comma (map minprint fields))

instance LMin TableField where
    minprint (ExpField e1 e2)    = brackets (minprint e1) <> equals <> minprint e2
    minprint (NamedField name e) = minprint name <> equals <> minprint e
    minprint (Field e)           = minprint e

instance LMin Block where
    minprint (Block stats ret) =
      case stats of
        [] -> ret'
        _  -> sep' (map minprint stats) <+> ret'
      where ret' = case ret of
                     Nothing -> empty
                     Just [fun@EFunDef{}] -> text "return" <+> minprint fun
                     Just e  -> (text "return" <+> cat' (List.intersperse comma (map minprint e)))

instance LMin FunName where
    minprint (FunName name s methods) = intercalate dot (map minprint $ name:s) <> method'
      where method' = case methods of
                        Nothing -> empty
                        Just m' -> char ':' <> minprint m'

instance LMin FunBody where
    minprint = minprintFunction Nothing

minprintFunction :: Maybe Doc -> FunBody -> Doc
minprintFunction funname (FunBody args vararg block) =
    header <> body <+> end
  where
    header = case funname of
               Nothing -> text "function" <> args'
               Just n  -> text "function" <+> n <> args'
    vararg' = if vararg then ["..."] else []
    args' = parens $ cat' (List.intersperse comma (map minprint (args ++ vararg')))
    body = minprint block
    end = text "end"

instance LMin FunCall where
    minprint (NormalFunCall pe arg)     = minprint pe <> minprint arg
    minprint (MethodCall pe method arg) = minprint pe <> colon <> minprint method <> minprint arg

instance LMin FunArg where
    minprint (Args [fun@EFunDef{}]) = parens (minprint fun)
    minprint (Args exps)   = parens (intercalate comma (map minprint exps))
    minprint (TableArg t)  = minprint t
    minprint (StringArg s) = text s

instance LMin Stat where
    minprint (Assign names vals)
        =   intercalate comma (map minprint names)
        <> equals
        <> intercalate comma (map minprint vals)
    minprint (FunCall funcall) = minprint funcall
    minprint (Label name)      = text "::" <> minprint name <> text "::"
    minprint Break             = text "break"
    minprint (Goto name)       = text "goto" <+> minprint name
    minprint (Do block)        = text "do" <+> minprint block <+> text "end"
    minprint (While guard e)
        =  nest 2 (text "while" <+> minprint guard <+> text "do" <+> minprint e)
       <+> text "end"
    minprint (Repeat block guard)
        =  (text "repeat" <+> minprint block)
        <+> (text "until" <+> minprint guard)

    minprint (If cases elsePart) = group (printIf cases elsePart)
      where
        printIf ((guard, block) : xs) e =
          nest 2 (text "if" <+> minprint guard <+> text "then" <+> minprint block) <+> printIf' xs e
        printIf [] _ =
          error $ "minprint: Trying to print invalid syntax:\n\t" ++
                  "if statement should have at least one case"

        printIf' [] Nothing  = text "end"
        printIf' [] (Just b) = nest 2 (text "else" <+> minprint b) <+> text "end"
        printIf' ((guard, block) : xs) e =
          nest 2 (text "elseif" <+> minprint guard <+> text "then" <+> minprint block) <+> printIf' xs e

    minprint (ForRange name e1 e2 e3 block)
        =   nest 2 (text "for" <+> minprint name <> equals <> minprint e1
                      <> comma <> minprint e2 <> e3' <+> text "do"
                      <+> minprint block)
        <+> text "end"
      where e3' = case e3 of
                    Nothing -> empty
                    Just e  -> comma <> minprint e

    minprint (ForIn names exps block)
        =   nest 2 (text "for" <+> intercalate comma (map minprint names) <+> text "in"
                     <+> intercalate comma (map minprint exps) <+> text "do"
                     <+> minprint block)
        <+> text "end"

    minprint (FunAssign name body) = minprintFunction (Just (minprint name)) body
    minprint (LocalFunAssign name body) = text "local" <+> minprintFunction (Just (minprint name)) body
    minprint (LocalAssign names exps)
        = text "local" <+> intercalate comma (map minprint names) <> exps'
      where exps' = case exps of
                      Nothing -> empty
                      Just es -> equals <> intercalate comma (map minprint es)
    minprint EmptyStat = text ";"
