module Main where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser.Parser
import Parser.Interpreter (runAndEvaluate)
import Parser.Util
import Prelude
import System.Environment (getArgs)
import Text.Parsec
import Parser.CompileToJs (compile)
 
main :: IO ()
main = do
  args <- getArgs 
  case args of
    ["repl"] -> repl
    ["run", fileName] -> TIO.readFile fileName >>= parseAndRunProgram
    ["compile", fileName] -> TIO.readFile fileName >>= parseAndCompileProgram
    _ -> putStrLn "Usage: repl / run [FILENAME] / compile [FILENAME]"

repl :: IO ()
repl = do
  input <- getLine
  if input == "q" then
    return ()
  else
    do
      parseAndRunProgram (T.pack input)
      repl

parseAndRunProgram :: T.Text -> IO ()
parseAndRunProgram input = do
  let maybeProg =  mapLeft show $ parse program "program" input
  case maybeProg of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right prog -> do
      result <- runAndEvaluate prog
      putStrLn (either ("Runtime error: " ++) show result)

parseAndCompileProgram :: T.Text -> IO ()
parseAndCompileProgram input = do
  let maybeProg =  mapLeft show $ parse program "program" input
  case maybeProg of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right prog -> do
      let result = compile prog
      TIO.putStrLn (either (\s -> T.pack $ "Compilation error: " ++ s) id result)