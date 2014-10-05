module Main(main) where

import System.Environment
import System.IO

import Lexer
import Parser

main = do
  (fileName:rest) <- getArgs
  fileHandle <- openFile fileName ReadMode
  programText <- hGetContents fileHandle
  putStrLn $ show programText
  let toks = strToToks programText
  putStrLn $ show toks
  let parsedMod = parseExpr $ strToToks programText
  hClose fileHandle
  putStrLn $ show parsedMod
