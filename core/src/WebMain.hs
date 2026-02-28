{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Core.WebREPL (evalProgram)

main :: IO ()
main = do
  src <- TIO.getContents
  case evalProgram src of
    Left err -> putStrLn $ "parse error:\n" ++ err
    Right out -> putStrLn out
