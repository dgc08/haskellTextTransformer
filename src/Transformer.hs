module Transformer(transform, transformerTable) where

import System.Environment (getArgs)
import System.IO (hGetContents, stdin, hPutStrLn, stderr)
import Control.DeepSeq (deepseq)

writeOutput :: String -> String -> IO ()
writeOutput dest =
  if dest == "-o"
    then putStr
  else
    writeFile dest

transform :: (String -> String) -> IO ()
transform transformer = do
  args <- getArgs
  if null args || (length args == 1 && head args == "-i")
    then do
      contents <- hGetContents stdin
      let input =  contents `deepseq` contents
      putStr $ transformer input
  else if length args >= 2
    then if head args == "-i"
          then do
            contents <- hGetContents stdin
            let input =  contents `deepseq` contents
            let transformed = transformer input
            writeOutput (args !! 1) transformed
          else do
            input <- readFile $ head args
            let transformed = transformer input
            writeOutput (args !! 1) transformed
  else do
    contents <- hGetContents stdin
    let input =  contents `deepseq` contents
    let transformed = transformer input
    writeOutput "-o" transformed

transformerTable :: (String -> (String -> String)) -> IO()
transformerTable table = do
  args <- getArgs
  if length args == 1
    then do putStr $ table (head args) "Transformer Library Error: No I/O provided\n"
  else if length args < 3
    then do
      hPutStrLn stderr "Transformer Library Error: No action/transformer (3rd argument) provided"
  else do
      transform $ table (args !! 2)
