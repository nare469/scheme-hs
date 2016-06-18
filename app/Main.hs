module Main where

import System.Environment
import Parser
import Evaluator
import DataTypes

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
