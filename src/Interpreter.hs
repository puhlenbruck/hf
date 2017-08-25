module Interpreter
    ( run
    ) where

import Data.Maybe
import Data.Word
import System.IO

run :: [Char] -> IO()
run = runProgram . mapMaybe parseCommand

data Command = DEBUG | NextCell | PrevCell | Increment | Decrement | Write | Read | ForwardJumpIfZero | BackWardJumpIfNonZero
    deriving (Show)

data DataPointer = DataPointer [Word8] Word8 [Word8]
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

runProgram :: [Command] -> IO()
runProgram [] = return ()
runProgram (x:xs) = loop (x : xs) (DataPointer [] 0 [])

loop :: [Command] -> DataPointer -> IO()
loop [] _ = return ()
loop (NextCell:xs) (DataPointer previous current []) = loop xs (DataPointer (current : previous) 0 [])
loop (NextCell:xs) (DataPointer previous current (n:ns)) = loop xs (DataPointer (current : previous) n ns)
loop (PrevCell:xs) (DataPointer [] current next) = loop xs (DataPointer [] 0 (current : next))
loop (PrevCell:xs) (DataPointer (p:ps) current next) = loop xs (DataPointer ps p (current : next))
loop (Increment:xs) (DataPointer previous current next) = loop xs (DataPointer previous (current + 1) next)
loop (Decrement:xs) (DataPointer previous current next) = loop xs (DataPointer previous (current - 1) next)
loop (Write:xs) (DataPointer previous current next) = putChar(toEnum . fromEnum $ current) >> hFlush stdout >> loop xs (DataPointer previous current next)
loop (DEBUG:xs) state = print state >> hFlush stdout >> loop xs state
