module Main where

import System.Environment
import Parser

main :: IO ()
main = do
    (arg:_) <- getArgs
    putStrLn $ readExpr arg
