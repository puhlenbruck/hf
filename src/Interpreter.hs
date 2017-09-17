module Interpreter
    ( run
    ) where

import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.IO

import DataPointer

run :: [Char] -> IO()
run = runProgram . Vector.fromList . mapMaybe parseCommand

data Command = DEBUG | NextCell | PrevCell | Increment | Decrement | Write | Read | ForwardJumpIfZero | BackWardJumpIfNonZero
    deriving (Show)

parseCommand :: Char -> Maybe Command
parseCommand '>' = Just NextCell
parseCommand '<' = Just PrevCell
parseCommand '+' = Just Increment
parseCommand '-' = Just Decrement
parseCommand '.' = Just Write
parseCommand ',' = Just Read
parseCommand '[' = Just ForwardJumpIfZero
parseCommand ']' = Just BackWardJumpIfNonZero
parseCommand '*' = Just DEBUG
parseCommand  _  = Nothing

runProgram :: Vector Command -> IO()
runProgram code
    | Vector.null code = return ()
    | otherwise = loop code 0 (DataPointer [] 0 [])

loop :: Vector Command -> Int -> DataPointer -> IO()
loop commands pc dataPointer
    | pc >= Vector.length commands = return ()
    | otherwise            = let (newPC, newDataPointer, action) = processCommand (commands ! pc) commands pc dataPointer
                             in action >> loop commands newPC newDataPointer

processCommand :: Command -> Vector Command -> Int -> DataPointer -> (Int, DataPointer, IO ())
processCommand NextCell  commands pc dataPointer = (pc + 1, nextCell dataPointer, return ())
processCommand PrevCell  commands pc dataPointer = (pc + 1, prevCell dataPointer, return ())
processCommand Increment commands pc dataPointer = (pc + 1, incrementCurrent dataPointer, return ())
processCommand Decrement commands pc dataPointer = (pc + 1, decrementCurrent dataPointer, return ())
processCommand Write     commands pc dataPointer = (pc + 1, dataPointer, putChar (toEnum . fromEnum $ currentCell dataPointer) >> hFlush stdout)
processCommand DEBUG     commands pc dataPointer = (pc + 1, dataPointer, print dataPointer >> hFlush stdout)
    {--
loop (NextCell:xs) (DataPointer previous current []) = loop xs (DataPointer (current : previous) 0 [])
loop (NextCell:xs) (DataPointer previous current (n:ns)) = loop xs (DataPointer (current : previous) n ns)
loop (PrevCell:xs) (DataPointer [] current next) = loop xs (DataPointer [] 0 (current : next))
loop (PrevCell:xs) (DataPointer (p:ps) current next) = loop xs (DataPointer ps p (current : next))
loop (Increment:xs) (DataPointer previous current next) = loop xs (DataPointer previous (current + 1) next)
loop (Decrement:xs) (DataPointer previous current next) = loop xs (DataPointer previous (current - 1) next)
loop (Write:xs) (DataPointer previous current next) = putChar(toEnum . fromEnum $ current) >> hFlush stdout >> loop xs (DataPointer previous current next)
loop (DEBUG:xs) state = print state >> hFlush stdout >> loop xs state
--}
