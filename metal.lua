-- MetaL, Meta Lua: a thin lispy meta programming layer on top of Lua,
-- inspired by Parenscript.

-- S-expression parser from https://github.com/zick/LuaLisp with slight
-- modifications.  Thanks zick!

local kLPar = '('
local kRPar = ')'
local kQuote = '"'
local kBQuote = '`'
local kComma = ','
local kAt = '@'

function isSpace(c)
   return c == ' ' or c == '\t' or c == '\r' or c == '\n'
end

function isDelimiter(c)
   return c == kLPar or c == kRPar or c == kQuote or isSpace(c)
end

function skipSpaces(str)
   for i = 1, string.len(str) do
      if not(isSpace(string.sub(str, i, i))) then
         return string.sub(str, i)
      end
   end
   return ''
end

function isNumChar(c)
   local zero = string.byte('0', 1, 1)
   local nine = string.byte('9', 1, 1)
   return zero <=  string.byte(c, 1, 1) and string.byte(c, 1, 1) <= nine
end

function toNum(c)
   return string.byte(c, 1, 1) - string.byte('0', 1, 1)
end

function makeNumOrSym(str)
   local i = 1
   local sign = 1
   if string.sub(str, 1, 1) == '-' then
      sign = -1
      i = 2
   end
   local is_num = false
   local num = 0
   for j = i, string.len(str) do
      c = string.sub(str, j, j)
      if isNumChar(c) then
         num = num * 10 + toNum(c)
         is_num = true
      else
         is_num = false
         break
      end
   end
   if is_num then return num * sign
   else return str end
end

function readAtom(str)
   local next = ''
   for i = 1, string.len(str) do
      if isDelimiter(string.sub(str, i, i)) then
         next = string.sub(str, i)
         str = string.sub(str, 1, i - 1)
         break
      end
   end
   return makeNumOrSym(str), next
end

local readList = nil

function read1(str)
   local str = skipSpaces(str)
   assert(str ~= '', "Empty input.")
   c = string.sub(str, 1, 1)
   if c == kRPar then return error(("invalid syntax: %s"):format(str))
   elseif c == kLPar then return readList(string.sub(str, 2))
   else return readAtom(str) end
end

-- Read backquote syntax, e.g.: `(foo ,bar ,@baz)
function readSpecial (str)
   local str = skipSpaces(str)
   assert(str ~= '', "Empty input.")
   c = string.sub(str, 1, 1)
   if c == kBQuote then
      local val, next = read1(string.sub(str, 2))
      return {"quote", val}, next
   elseif c == kComma then
      if string.sub(str, 2, 2) == kAt then
         local val, next = read1(string.sub(str, 3))
         return {"splice", val}, next
      else 
         local val, next = read1(string.sub(str, 2))
         return {"unquote", val}, next
      end
   else
      return read1(str)
   end
end

readList = function (str)
   local ret = {}
   while true do
      str = skipSpaces(str)
      if str == '' then error('unbalanced parenthesis')
      elseif string.sub(str, 1, 1) == kRPar then break
      else
         local elm, next = readSpecial(str)
         ret[#ret+1] = elm
         str = next
      end
   end
   return ret, string.sub(str, 2)
end

function read (str)
   local val, _ = readSpecial(str)
   return val
end


-- “Lisp” to Lua compiler. `builtin' contains special forms, `macro'
-- contains macro functions.

builtin = {}

macro = {}

function setMacro (n, m)
   assert(not builtin[n], ("`%s' is a built-in."):format(n))
   macro[n] = m
end

function compileAtom (x, quoted)
   local type = type(x)
   if type == 'string' then
      if quoted then return ("%q"):format(x)
      else           return ("%s"):format(x) end
   elseif type == 'number' then
      return ("%s"):format(x)
   else
      error(("%s is not an atom."):format(x))
   end
end

function compileList (d, f, e)
   local l = ""
   for i = 1, #e do
      l = l..f(e[i])
      if i < #e then l = l..d end
   end
   return l
end

compile = nil
function compileFcall (f, args)
   return ("%s(%s)"):format(f, compileList(", ", compile, args))
end

function compileInvocation (op, ...)
   if builtin[op] then return builtin[op](...)
   else return compileFcall(op, {...}) end
end

function macroExpand (op, ...)
   local x
   if macro[op] then
      x = macroExpand(unpack(macro[op](...)))
   else
      x = { op }
      for _, arg in ipairs({...}) do
         if type(arg) == 'table' then x[#x+1] = macroExpand(unpack(arg))
         else x[#x+1] = arg end
      end
   end
   return x
end

function compile (x)
   if type(x) == 'table' then
      return compileInvocation(unpack(macroExpand(unpack(x))))
   else
      return compileAtom(x)
   end
end


-- Built-ins: Provide special forms for Lua constructs such as
-- “for...in”, “if...then”, etc.

function compileFunction (p, ...)
   return (
      "(%s) %s end"
          ):format(compileList(", ", compileAtom, p),
                   compileList("; ", compile, {...}))
end

-- (function ...)
builtin["function"] = function (x, ...)
   if type(x) == "string" then
      return ("function %s %s"):format(x, compileFunction(...))
   else
      return ("function %s"):format(compileFunction(x, ...))
   end
end

-- (return ...)
builtin["return"] = function (...)
   return ("return %s"):format(compileList(", ", compile, {...}))
end

-- Literals
builtin["vector"] = function (...)
   return ("{%s}"):format(compileList(", ", compile, {...}))
end

function identity (...) return ... end

builtin["table"] = function (...)
   local args, pairs = {...}, {}
   assert(#args%2 == 0, "Odd number of arguments to `table'.")
   for i = 2, #args, 2 do
      pairs[i/2] = ("%s = %s"):format(compileAtom(args[i-1]),
                                      compile(args[i]))
   end
   return ("{%s}"):format(compileList(", ", identity, pairs))
end

-- (= vars ...)
builtin["="] = function (vars, ...)
   local assign = "%s = %s"
   if type(vars) == "table" then
      return assign:format(compileList(", ", compileAtom, vars),
                           compileList(", ", compile, {...}))
   else
      return assign:format(compileAtom(vars),
                           compileList(", ", compile, {...}))
   end
end

-- (local vars ...)
builtin["local"] = function (vars, ...)
   local assign = "local %s = %s"
   if type(vars) == "table" then
      return assign:format(compileList(", ", compileAtom, vars),
                           compileList(", ", compile, {...}))
   elseif vars == "function" then
      return ("local %s"):format(builtin["function"](...))
   else
      return assign:format(compileAtom(vars),
                           compileList(", ", compile, {...}))
   end
end

-- (do ...)
builtin["do"] = function (...)
   return ("do %s end"):format(compileList("; ", compile, {...}))
end

-- (if c t e)
builtin["if"] = function (c, t, e)
   if e then return (
         "if %s then return %s else return %s end"
                    ):format(compile(c), compile(t), compile(e))
   else return (
         "if %s then return %s end"
               ):format(compile(c), compile(t))
   end
end

-- Infix operators: and, or, ==, ~=, >, <, >=, <=, +, -, *, /, %, ^
function makeInfixBuiltin (infix)
   local d = (" %s "):format(infix)
   return function (...)
      return ("(%s)"):format(compileList(d, compile, {...}))
   end
end
for _, infix in ipairs({"and", "or",
                        "==", "~=",
                        ">", "<", ">=", "<=",
                        "+", "-", "*", "/", "%", "^"}) do
   builtin[infix] = makeInfixBuiltin(infix)
end

-- (for ...)
builtin["for"] = function (i, ...)
   return (
      "for %s, %s in %s do %s end"
          ):format(compile(i[1]),
                   compile(i[2]),
                   compile(i[3]),
                   compileList("; ", compile, {...}))
end

-- (defmacro name (...) ...), used to define macros.
builtin["defmacro"] = function (name, ...)
   return ("setMacro(%q, %s)"):format(name, compile({"function", ...}))
end


-- The `quote' built-in implements `quote'/`unquote'/`splice' for
-- backquote syntax, e.g.:
--   `(foo ,bar ,@baz) => (quote (foo (unquote bar) (splice baz)))

function splice (...)
   local a = {}
   for _, v in ipairs({...}) do
      if type(v) == "table" then
         for _, x in ipairs(v) do
            a[#a+1] = x
         end
      else
         a[#a+1] = v
      end
   end
   return a
end

function vQuote (x)
   if type(x) == "table" then
      if x[1] == "unquote" then
         return ("{%s}"):format(compile(x[2]))
      elseif x[1] == "splice" then
         return compile(x[2])
      else
         return ("{splice(%s)}"):format(compileList(", ", vQuote, x))
      end
   else
      return compileAtom(x, true)
   end
end

builtin["quote"] = function (x)
   if type(x) == "table" then
      return ("splice(%s)"):format(compileList(", ", vQuote, x))
   else
      return compileAtom(x, true)
   end
end


-- High level interface glue...

-- https://github.com/SnabbCo/snabbswitch/blob/443081a14f7c07d425d527b45efc741590afa70b/src/core/lib.lua#L52-L58
function readfile (filename, what)
   local f = io.open(filename, "r")
   if f == nil then error("Unable to open file: " .. filename) end
   local value = f:read(what)
   f:close()
   return value
end

function compileFile (filename)
   return compile(read(readfile(filename, "*a")))
end

function loadFile (filename)
   assert(loadstring(compileFile(filename), filename))()
end

function metal (str)
   assert(loadstring(compile(read(str)), str))()
end
