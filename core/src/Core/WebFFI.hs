{-# LANGUAGE ForeignFunctionInterface #-}
module Core.WebFFI where

import Foreign.C.String (CString, newCString)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import Core.WebREPL (evalProgram, evalExpr)

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
