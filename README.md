# lua-wane

A tool to shrink the size of Lua source code by removing white space
and comments. This is based on work done in
[language-lua](https://github.com/osa1/language-lua) and depends on
it.

# Usage

To minify a file and write it somewhere else:

```
lua-wane input-file.lua mifified.lua
```

To minify a file and write it to `stdout`:

```
lua-wane input-file.lua -
```

# Installation

This project uses
[stack](http://docs.haskellstack.org/en/stable/README/) for build
management. If you have stack installed, all you need to do is the
following:

```
git clone https://github.com/sw17ch/lua-wane.git
cd lua-wane
stack install
```
