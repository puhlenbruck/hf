module Main where

import System.Environment
import Interpreter

import qualified Data.ByteString.Lazy as BS


main :: IO ()
main = do
    [program] <- getArgs
    input <- BS.getContents
    run program input
