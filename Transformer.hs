module Transformer where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Environment (getArgs)
import System.IO (hGetContents, stdin, hPutStrLn, stderr)

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
      input <- BSC.hGetContents stdin
      putStr (transformer (BSC.unpack input))
  else if length args >= 2
    then if head args == "-i"
          then do
            input <- BSC.hGetContents stdin
            let transformed = transformer (BSC.unpack input)
            writeOutput (args !! 1) transformed
          else do
            input <- BSC.readFile (head args)
            let transformed = transformer (BSC.unpack input)
            writeOutput (args !! 1) transformed
  else do
    input <- BSC.hGetContents stdin
    let transformed = transformer (BSC.unpack input)
    writeOutput "-o" transformed

transformerTable :: (String -> (String -> String)) -> IO()
transformerTable table = do
  args <- getArgs
  if length args < 3
    then do
      hPutStrLn stderr "No action/transformer (3rd argument) provided"
    else do
      transform $ table (args !! 2)
