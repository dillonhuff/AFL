module Main(main) where

import Language.Haskell.Exts.Parser
import System.Environment
import System.IO

main = do
  (fileName:rest) <- getArgs
  fileHandle <- openFile fileName ReadMode
  programText <- hGetContents fileHandle
  let parsedMod = parseModule programText
  hClose fileHandle
  putStrLn $ show parsedMod
