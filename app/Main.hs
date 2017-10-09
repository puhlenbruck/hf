{- |
Module      :  Main
Description :  Entry point.  Parses command line arguments and runs program
Copyright   :  (c) Peter Uhlenbruck 2017
License     :  MIT Licence
-}
module Main where

import System.Environment
import Interpreter
import System.IO as IO
import Data.ByteString.Lazy as BS


main :: IO ()
main = do
    args <- getArgs
    let (program, inputs) = parseArgs args
    ins <- sequence inputs
    command <- program
    sequence_ $ fmap (run command) ins

{-
The Interpreter will read the program from the file provided in the first argument,
or as a string passed in the the -c option.

The program will be run for each file provided or use stdin as input if no
input file provided in the argument list.
-}
parseArgs :: [String] -> (IO String, [IO ByteString])
parseArgs ["-c", command] = (return command, [BS.getContents])
parseArgs ("-c" : command : inputFiles) = (return command, fmap BS.readFile inputFiles)
parseArgs [file] = (IO.readFile file, [BS.getContents])
parseArgs (file : inputFiles) = (IO.readFile file, fmap BS.readFile inputFiles)
