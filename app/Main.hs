module Main where

import System.Environment
import Interpreter


main :: IO ()
main = do
    [input] <- getArgs
    run input
