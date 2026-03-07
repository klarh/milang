{-# LANGUAGE OverloadedStrings #-}
module Core.Prelude (preludeBindings, ffiNamespace) where

import Data.Text (Text)
import qualified Data.Text as T
import Core.Syntax
import Core.Parser (parseProgram)

-- | The milang prelude as source text. Parsed at compiler startup.
-- | Bootstrap prelude — strict evaluation only (no #/$ quote/splice yet).
preludeSrc :: Text
preludeSrc = T.unlines
  [ "Bool = {True; False}"
  , "List = {Nil; Cons head tail}"
  , "Maybe = {Nothing; Just val}"
  , "eq a b = a -> _ = a == b"
  , "if cond #t #e = (truthy cond) -> 0 = $e; _ = $t"
  , "truthy val = val -> False = 0; Nil = 0; Nothing = 0; 0 = 0; \"\" = 0; _ = 1"
  , "toString val = val -> True = \"True\"; False = \"False\"; Nil = \"[]\"; Nothing = \"Nothing\"; Just x = \"Just(\" + toString x + \")\"; _ = _toString val"
  , "id x = x"
  , "const x y = x"
  , "flip f a b = f b a"
  , "null lst = lst -> Nil = 1; Nothing = 1; _ = 0"
  , "len x = x -> Nil = 0; Cons h t = 1 + len t; Nothing = 0; Just v = 1"
  , "head lst = lst -> Nil = Nothing; Cons h t = Just h"
  , "tail lst = lst -> Nil = Nothing; Cons h t = Just t"
  , "fold f acc x = x -> Nil = acc; Cons h t = fold f (f acc h) t; Nothing = acc; Just v = f acc v"
  , "map f x = x -> Nil = []; Cons h t = Cons (f h) (map f t); Nothing = Nothing; Just v = Just (f v)"
  , "filter f x = x -> Nil = []; Cons h t = if (f h) (Cons h (filter f t)) (filter f t); Nothing = Nothing; Just v = if (f v) (Just v) Nothing"
  , "concat a b = a -> Nil = b; Cons h t = Cons h (concat t b); Nothing = b; Just v = Just v"
  , "push lst x = concat lst [x]"
  , "at lst i = lst -> Nil = Nothing; Cons h t = if (i == 0) (Just h) (at t (i - 1))"
  , "at' i lst = at lst i"
  , "sum lst = fold (\\acc x -> acc + x) 0 lst"
  , "product lst = fold (\\acc x -> acc * x) 1 lst"
  , "any f x = fold (\\acc v -> if (f v) 1 acc) 0 x"
  , "all f x = fold (\\acc v -> if (f v) acc 0) 1 x"
  , "contains lst x = any (\\el -> el == x) lst"
  , "range start end = range_helper start end []"
  , "range_helper start end acc = if (start >= end) acc (range_helper start (end - 1) (Cons (end - 1) acc))"
  , "zip a b = if (null a) [] (if (null b) [] (Cons [a.head, b.head] (zip a.tail b.tail)))"
  , "last lst = lst -> Nil = Nothing; Cons h t = if (null t) (Just h) (last t)"
  , "init lst = lst -> Nil = Nothing; Cons h t = if (null t) (Just []) (Just (Cons h (fold (flip const) [] (init t))))"
  , "reverse lst = fold (\\acc x -> Cons x acc) [] lst"
  , "take n lst = if (n == 0) [] (take_inner n lst)"
  , "take_inner n lst = lst -> Nil = []; Cons h t = Cons h (take (n - 1) t)"
  , "drop n lst = if (n == 0) lst (drop_inner n lst)"
  , "drop_inner n lst = lst -> Nil = []; Cons h t = drop (n - 1) t"
  , "enumerate lst = zip (range 0 (len lst)) lst"
  , "join sep lst = lst -> Nil = \"\"; Cons h t = if (null t) h (h + sep + join sep t)"
  , "abs x = if (x < 0) (0 - x) x"
  , "neg x = 0 - x"
  , "min a b = if (a < b) a b"
  , "max a b = if (a > b) a b"
  , "not x = if x 0 1"
  , "(|>) x f = f x"
  , "(>>) f g x = g (f x)"
  , "(<<) f g x = f (g x)"
  , "(&&) a #b = if a ($b) 0"
  , "(||) a #b = if a 1 ($b)"
  -- Type aliases
  , "Int = Int' 0"
  , "UInt = UInt' 0"
  , "Float = Float' 64"
  , "Byte = UInt' 8"
  -- Trait annotations
  , "console :~ [console]"
  , "io :~ [console]"
  , "fs :~ [fs.read, fs.write]"
  , "process :~ [process]"
  , "env :~ [env]"
  -- IO function trait annotations (so rebound names keep effect tracking)
  , "println :~ [console]"
  , "print :~ [console]"
  , "readLine :~ [console]"
  -- Math type annotations
  , "float :: Int : Float"
  , "round :: Float : Int"
  , "floor :: Float : Int"
  , "ceil :: Float : Int"
  -- Type annotations for prelude functions
  , "toString :: a : Str"
  , "eq :: a : a : Num"
  , "truthy :: a : Num"
  , "id :: a : a"
  , "const :: a : b : a"
  , "flip :: (a : b : c) : b : a : c"
  , "null :: List : Num"
  , "null :: Maybe : Num"
  , "len :: a : Num"
  , "head :: List : Maybe"
  , "tail :: List : Maybe"
  , "fold :: (a : b : a) : a : List : a"
  , "fold :: (a : b : a) : a : Maybe : a"
  , "map :: (a : b) : List : List"
  , "map :: (a : b) : Maybe : Maybe"
  , "filter :: (a : Num) : List : List"
  , "filter :: (a : Num) : Maybe : Maybe"
  , "concat :: List : List : List"
  , "concat :: Maybe : Maybe : Maybe"
  , "push :: List : a : List"
  , "at :: List : Num : Maybe"
  , "at' :: Num : List : Maybe"
  , "sum :: List : Num"
  , "product :: List : Num"
  , "any :: (a : Num) : List : Num"
  , "any :: (a : Num) : Maybe : Num"
  , "all :: (a : Num) : List : Num"
  , "all :: (a : Num) : Maybe : Num"
  , "contains :: List : a : Num"
  , "range :: Num : Num : List"
  , "zip :: List : List : List"
  , "last :: List : Maybe"
  , "init :: List : Maybe"
  , "reverse :: List : List"
  , "take :: Num : List : List"
  , "drop :: Num : List : List"
  , "enumerate :: List : List"
  , "join :: Str : List : Str"
  , "abs :: a : a"
  , "neg :: Num : Num"
  , "min :: a : a : a"
  , "max :: a : a : a"
  , "not :: a : Num"
  -- IO function types
  , "println :: a : Num"
  , "print :: a : Num"
  -- Operator types
  , "(+) :: a : a : a"
  , "(-) :: a : a : a"
  , "(*) :: a : a : a"
  , "(/) :: a : a : a"
  , "(%) :: Int : Int : Int"
  , "(**) :: a : Int : a"
  , "(==) :: a : a : Int"
  , "(/=) :: a : a : Int"
  , "(<) :: a : a : Int"
  , "(>) :: a : a : Int"
  , "(<=) :: a : a : Int"
  , "(>=) :: a : a : Int"
  , "(|>) :: a : (a : b) : b"
  , "(>>) :: (a : b) : (b : c) : a : c"
  , "(<<) :: (b : c) : (a : b) : a : c"
  , "(&&) :: a : a : Num"
  , "(||) :: a : a : Num"
  -- Boosted prelude
  , "flatMap f x = x -> Nil = []; Cons h t = concat (f h) (flatMap f t); Nothing = Nothing; Just v = f v"
  , "foldRight f z lst = lst -> Nil = z; Cons h t = f h (foldRight f z t)"
  , "scanLeft f acc lst = Cons acc (scanLeft_tail f acc lst)"
  , "scanLeft_tail f acc lst = lst -> Nil = []; Cons h t = scanLeft f (f acc h) t"
  , "unfold f seed = unfold_step f (f seed)"
  , "unfold_step f result = result -> Nothing = []; Just pair = Cons pair.fst (unfold f pair.snd)"
  , "partition_step pred acc x = if (pred x) (partition_yes acc x) (partition_no acc x)"
  , "partition_yes acc x = {yes = Cons x acc.yes; no = acc.no}"
  , "partition_no acc x = {yes = acc.yes; no = Cons x acc.no}"
  , "partition pred lst = fold (partition_step pred) {yes = []; no = []} lst"
  , "iterate f x = Cons x ~(iterate f (f x))"
  , "curry f a b = f {fst = a; snd = b}"
  , "uncurry f pair = f pair.fst pair.snd"
  , "on f g a b = f (g a) (g b)"
  , "fst pair = pair.fst"
  , "snd pair = pair.snd"
  , "sortBy cmp lst = lst -> Nil = []; Cons pivot rest = concat (concat (sortBy cmp (filter (\\x -> cmp x pivot < 0) rest)) [pivot]) (sortBy cmp (filter (\\x -> cmp x pivot >= 0) rest))"
  , "sort lst = sortBy (\\a b -> if (a < b) (0 - 1) (if (a == b) 0 1)) lst"
  , "nubBy eq lst = lst -> Nil = []; Cons h t = Cons h (nubBy eq (filter (\\x -> not (eq h x)) t))"
  , "nub lst = nubBy (\\a b -> a == b) lst"
  , "groupBy eq lst = lst -> Nil = []; Cons h t = Cons (Cons h (filter (\\x -> eq h x) t)) (groupBy eq (filter (\\x -> not (eq h x)) t))"
  -- Boosted prelude type annotations
  , "flatMap :: (a : List) : List : List"
  , "flatMap :: (a : Maybe) : Maybe : Maybe"
  , "foldRight :: (a : b : b) : b : List : b"
  , "scanLeft :: (a : b : a) : a : List : List"
  , "iterate :: (a : a) : a : List"
  , "sortBy :: (a : a : Num) : List : List"
  , "sort :: List : List"
  , "nubBy :: (a : a : Num) : List : List"
  , "nub :: List : List"
  , "groupBy :: (a : a : Num) : List : List"
  -- String utilities
  , "startsWith str prefix = slice str 0 (len prefix) == prefix"
  , "endsWith str suffix = slice str (len str - len suffix) (len str) == suffix"
  , "words str = split str \" \""
  , "lines str = split str \"\\n\""
  , "unwords lst = join \" \" lst"
  , "unlines lst = join \"\\n\" lst"
  -- String utility type annotations
  , "startsWith :: Str : Str : Num"
  , "endsWith :: Str : Str : Num"
  , "words :: Str : List"
  , "lines :: Str : List"
  , "unwords :: List : Str"
  , "unlines :: List : Str"
  -- Monad system: AST constructors for quote/splice metaprogramming
  , "__AstNode = {App fn arg; Var name; Lam param body}"
  -- Monad desugaring helpers
  , "_monad_then field rest = App {fn = App {fn = Var {name = \"_m_bind\"}; arg = field.val}; arg = Lam {param = \"_\"; body = rest}}"
  , "_monad_bind field rest = App {fn = App {fn = Var {name = \"_m_bind\"}; arg = field.val}; arg = Lam {param = field.name; body = rest}}"
  , "_monad_go fields = fields -> Nil = Var {name = \"_m_ret\"}; Cons field (Nil) = App {fn = Var {name = \"_m_ret\"}; arg = field.val}; Cons field rest = if (startsWith field.name \"_stmt_\") (_monad_then field (_monad_go rest)) (_monad_bind field (_monad_go rest))"
  -- monad: takes {bind, return} record, returns block processor via quote/splice
  , "monad defn #block = $(_monad_go block.fields)\n  _m_bind = defn.bind\n  _m_ret = defn.return"
  -- Maybe monad: short-circuits on Nothing
  , "_maybe_bind val f = val -> Nothing = Nothing; Just {_0 = x} = f x"
  , "maybe = monad ({bind = _maybe_bind; return = \\v -> Just v})"
  -- Block-to-list collector
  , "values #block = _collect_go block.fields"
  , "_collect_go fields = fields -> Nil = Nil; Cons field rest = Cons ($field.val) (_collect_go rest)"
  ]

-- | FFI descriptor builder functions for the C backend.
-- These are exposed as the 'ffi' object passed to import' annotate functions.
ffiFunctions :: Text
ffiFunctions = T.unlines
  [ "struct name = {_ffi = \"struct\"; name = name; fields = []}"
  , "field name ctype desc = desc <- {fields = Cons {head = {name = name; ctype = ctype}; tail = desc.fields}}"
  , "out func = {_ffi = \"out\"; func = func; params = []}"
  , "param index ctype desc = desc <- {params = Cons {head = {index = index; ctype = ctype}; tail = desc.params}}"
  , "opaque name = {_ffi = \"opaque\"; name = name; accessors = []}"
  , "accessor path ctype desc = desc <- {accessors = Cons {head = {name = path; ctype = ctype}; tail = desc.accessors}}"
  ]

-- | Build the ffi namespace for the C backend.
ffiNamespace :: Expr
ffiNamespace = case parseProgram "<ffi>" ffiFunctions of
  Right (Namespace bs) -> Namespace bs
  Right _  -> error "ffi functions did not parse as Namespace"
  Left err -> error $ "ffi parse error: " ++ show err

-- | Parse the prelude source into bindings.
preludeBindings :: [Binding]
preludeBindings = case parseProgram "<prelude>" preludeSrc of
  Right (Namespace bs) -> bs
  Right _  -> error "prelude did not parse as Namespace"
  Left err -> error $ "prelude parse error: " ++ show err
