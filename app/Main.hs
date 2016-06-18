module Main where

import Control.Monad
import System.Environment
import Parser
import Evaluator
import Error

main :: IO ()
main = do
    (args:_) <- getArgs
    evaled <- return $ liftM show $ readExpr args >>= eval
    putStrLn $ extractValue $ trapError evaled
