{-# LANGUAGE ForeignFunctionInterface #-}
module Core.WebFFI where

import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (castPtr)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char (ord)
import Data.List (intercalate)
import Text.Megaparsec (errorBundlePretty)

import Core.WebREPL (evalProgram, evalExpr)
import Core.Parser (parseProgramWithMain)
import Core.Reduce (reduce, emptyEnv)
import Core.Syntax

-- | Read code from a preopened file named "input.mi" and evaluate it.
-- Returns a freshly allocated C string with either the result or an error
-- prefixed with "ERR:".

eval_file_c :: IO CString
eval_file_c = do
  src <- TIO.readFile "input.mi"
  -- First try parsing as a single expression (common REPL usage). If that fails,
  -- fall back to parsing as a full program (top-level bindings).
  case evalExpr src of
    Right out -> newCString out
    Left _ -> case evalProgram src of
      Left err -> newCString ("ERR:" ++ err)
      Right out -> newCString out

foreign export ccall eval_file_c :: IO CString

-- | Parse the input.mi file into an AST and return JSON representation.
parse_file_c :: IO CString
parse_file_c = do
  src <- TIO.readFile "input.mi"
  case parseProgramWithMain "<input>" src of
    Left err -> newCString ("ERR:" ++ errorBundlePretty err)
    Right expr -> newCString (exprToJson expr)

foreign export ccall parse_file_c :: IO CString

-- | Run the reducer on input.mi and return the reduced AST as JSON.
reduce_file_c :: IO CString
reduce_file_c = do
  src <- TIO.readFile "input.mi"
  case parseProgramWithMain "<input>" src of
    Left err -> newCString ("ERR:" ++ errorBundlePretty err)
    Right expr ->
      let (reduced, _ws) = reduce emptyEnv expr
      in newCString (exprToJson reduced)

foreign export ccall reduce_file_c :: IO CString

-- | Parse a C string (pointer) and return JSON AST
parse_str_c :: CString -> IO CString
parse_str_c cstr = do
  s <- peekCString cstr
  case parseProgramWithMain "<input>" (T.pack s) of
    Left err -> newCString ("ERR:" ++ errorBundlePretty err)
    Right expr -> newCString (exprToJson expr)

foreign export ccall parse_str_c :: CString -> IO CString

-- | Reduce from a C string (pointer) and return reduced AST JSON
reduce_str_c :: CString -> IO CString
reduce_str_c cstr = do
  s <- peekCString cstr
  case parseProgramWithMain "<input>" (T.pack s) of
    Left err -> newCString ("ERR:" ++ errorBundlePretty err)
    Right expr -> let (reduced, _ws) = reduce emptyEnv expr in newCString (exprToJson reduced)

foreign export ccall reduce_str_c :: CString -> IO CString

-- | Allocate `n` bytes in the Haskell heap and return pointer (CString).
-- JS can call this to get a writable buffer in wasm memory, write bytes and
-- then pass the pointer to the parse_str_c/reduce_str_c functions.
alloc_bytes :: CSize -> IO CString
alloc_bytes (CSize n) = do
  p <- mallocBytes (fromIntegral n)
  return (castPtr p)

foreign export ccall alloc_bytes :: CSize -> IO CString

-- === JSON serialization of the Core.Syntax AST (minimal) ===

escapeJSON :: T.Text -> String
escapeJSON t = '"' : (concatMap escChar $ T.unpack t) ++ "\""
  where
    escChar '"' = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\r' = "\\r"
    escChar '\t' = "\\t"
    escChar c
      | ord c < 0x20 = let h = showHex4 (ord c) in "\\u" ++ h
      | otherwise = [c]

showHex4 :: Int -> String
showHex4 n = let h = showHex n "" in replicate (4 - length h) '0' ++ h

-- Simple hex conversion
showHex :: Int -> ShowS
showHex 0 = showString "0"
showHex n = showString (go n)
  where
    go 0 = ""
    go m = let (q,r) = m `divMod` 16
               digit = "0123456789abcdef" !! r
           in go q ++ [digit]

-- Convert Domain to string
domainToStr :: Domain -> String
domainToStr Value = "Value"
domainToStr Lazy  = "Lazy"
domainToStr Type  = "Type"
domainToStr Trait = "Trait"
domainToStr Doc   = "Doc"
domainToStr Parse = "Parse"

listToJson :: [String] -> String
listToJson xs = "[" ++ intercalate "," xs ++ "]"

bindingToJson :: Binding -> String
bindingToJson (Binding dom name params body _pos) =
  let paramsJson = listToJson (map (\t -> escapeJSON t) params)
  in "{" ++ intercalate "," [
       "\"domain\":" ++ escapeJSON (T.pack $ domainToStr dom),
       "\"name\":" ++ escapeJSON name,
       "\"params\":" ++ paramsJson,
       "\"body\":" ++ exprToJson body
     ] ++ "}"

altToJson :: Alt -> String
altToJson (Alt pat mg body) =
  "{" ++ intercalate "," ["\"pat\":" ++ patToJson pat,
                            "\"guard\":" ++ maybe "null" exprToJson mg,
                            "\"body\":" ++ exprToJson body] ++ "}"

patToJson :: Pat -> String
patToJson p = case p of
  PVar v -> "{\"tag\":\"PVar\",\"name\":" ++ escapeJSON v ++ "}"
  PLit e -> "{\"tag\":\"PLit\",\"expr\":" ++ exprToJson e ++ "}"
  PRec t fs -> "{\"tag\":\"PRec\",\"name\":" ++ escapeJSON t ++
                ",\"fields\": [" ++ intercalate "," (map (\(f,pat)->"[" ++ escapeJSON f ++ "," ++ patToJson pat ++ "]") fs) ++ "]}"
  PList ps mr -> "{\"tag\":\"PList\",\"elems\": " ++ listToJson (map patToJson ps) ++
                 ",\"rest\": " ++ maybe "null" escapeJSON mr ++ "}"
  PWild -> "{\"tag\":\"PWild\"}"

exprToJson :: Expr -> String
exprToJson e = case e of
  IntLit n -> "{\"tag\":\"IntLit\",\"value\":" ++ show n ++ "}"
  FloatLit d -> "{\"tag\":\"FloatLit\",\"value\":" ++ show d ++ "}"
  SizedInt n w s -> "{\"tag\":\"SizedInt\",\"value\":" ++ show n ++ ",\"width\":" ++ show w ++ ",\"signed\":" ++ show s ++ "}"
  SizedFloat d w -> "{\"tag\":\"SizedFloat\",\"value\":" ++ show d ++ ",\"width\":" ++ show w ++ "}"
  StringLit s -> "{\"tag\":\"StringLit\",\"value\":" ++ escapeJSON s ++ "}"
  Name n -> "{\"tag\":\"Name\",\"name\":" ++ escapeJSON n ++ "}"
  BinOp op l r -> "{\"tag\":\"BinOp\",\"op\":" ++ escapeJSON op ++ ",\"left\":" ++ exprToJson l ++ ",\"right\":" ++ exprToJson r ++ "}"
  App f x -> "{\"tag\":\"App\",\"f\":" ++ exprToJson f ++ ",\"x\":" ++ exprToJson x ++ "}"
  Lam p b -> "{\"tag\":\"Lam\",\"param\":" ++ escapeJSON p ++ ",\"body\":" ++ exprToJson b ++ "}"
  Record tag bs -> "{\"tag\":\"Record\",\"name\":" ++ escapeJSON tag ++ ",\"bindings\": [" ++ intercalate "," (map bindingToJson bs) ++ "]}"
  FieldAccess e' f -> "{\"tag\":\"FieldAccess\",\"expr\":" ++ exprToJson e' ++ ",\"field\":" ++ escapeJSON f ++ "}"
  Namespace bs -> "{\"tag\":\"Namespace\",\"bindings\": [" ++ intercalate "," (map bindingToJson bs) ++ "]}"
  Case scr alts -> "{\"tag\":\"Case\",\"scrut\":" ++ exprToJson scr ++ ",\"alts\": [" ++ intercalate "," (map altToJson alts) ++ "]}"
  Thunk t -> "{\"tag\":\"Thunk\",\"expr\":" ++ exprToJson t ++ "}"
  ListLit es -> "{\"tag\":\"ListLit\",\"items\": [" ++ intercalate "," (map exprToJson es) ++ "]}"
  With e' bs -> "{\"tag\":\"With\",\"expr\":" ++ exprToJson e' ++ ",\"bindings\": [" ++ intercalate "," (map bindingToJson bs) ++ "]}"
  Import p -> "{\"tag\":\"Import\",\"path\":" ++ escapeJSON p ++ "}"
  Quote q -> "{\"tag\":\"Quote\",\"expr\":" ++ exprToJson q ++ "}"
  Splice s -> "{\"tag\":\"Splice\",\"expr\":" ++ exprToJson s ++ "}"
  CFunction hdr name _ret _params _std -> "{\"tag\":\"CFunction\",\"header\":" ++ escapeJSON hdr ++ ",\"name\":" ++ escapeJSON name ++ "}"
  Error msg -> "{\"tag\":\"Error\",\"msg\":" ++ escapeJSON msg ++ "}"
