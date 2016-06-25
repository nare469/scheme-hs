module Main where

import System.Environment
import Repl

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            otherwise -> putStrLn "Invalid number of arguments"
