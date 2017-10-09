{- |
Module      :  Interpreter
Description :  Main interpreter logic.  Tokenizes and runs commands.
Copyright   :  (c) Peter Uhlenbruck 2017
License     :  MIT Licence
-}
module Interpreter
    ( run
    ) where

import Data.Word
import Prelude hiding (head, tail)
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.IO
import Data.ByteString.Lazy

import DataPointer

run :: [Char] -> ByteString -> IO()
run program input = runProgram (Vector.fromList $ mapMaybe parseCommand program) input

data Command = DEBUG | NextCell | PrevCell | Increment | Decrement | Write | Read | ForwardJumpIfZero | BackWardJumpIfNonZero
    deriving (Show, Eq)

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

runProgram :: Vector Command -> ByteString -> IO()
runProgram code input
    | Vector.null code = return ()
    | otherwise = loop code 0 (DataPointer [] 0 []) input

loop :: Vector Command -> Int -> DataPointer -> ByteString -> IO()
loop commands pc dataPointer input
    | pc >= Vector.length commands = return ()
    | otherwise = let (newPC, newDataPointer, newInput, action) = processCommand (commands ! pc) commands pc dataPointer input
                  in action >> loop commands newPC newDataPointer newInput

processCommand :: Command -> Vector Command -> Int -> DataPointer -> ByteString -> (Int, DataPointer, ByteString, IO ())
processCommand NextCell  commands pc dataPointer input = (pc + 1, nextCell dataPointer, input, return ())
processCommand PrevCell  commands pc dataPointer input = (pc + 1, prevCell dataPointer, input, return ())
processCommand Increment commands pc dataPointer input = (pc + 1, incrementCurrent dataPointer, input, return ())
processCommand Decrement commands pc dataPointer input = (pc + 1, decrementCurrent dataPointer, input, return ())
processCommand Write     commands pc dataPointer input = (pc + 1, dataPointer, input, putChar (toEnum . fromEnum $ currentCell dataPointer) >> hFlush stdout)
processCommand Read      commands pc dataPointer input = let (newValue, newInput) = readByteIfStreamOpen (currentCell dataPointer) input
                                                         in (pc + 1, setCurrent dataPointer newValue, newInput, return ())
processCommand ForwardJumpIfZero commands pc dataPointer input | currentCell dataPointer == 0 = (forwardJump commands pc, dataPointer, input, return ())
                                                               | otherwise = (pc + 1, dataPointer, input, return ())
processCommand BackWardJumpIfNonZero commands pc dataPointer input | currentCell dataPointer /= 0 = (backwardJump commands pc, dataPointer, input, return ())
                                                                   | otherwise = (pc + 1, dataPointer, input, return ())

processCommand DEBUG     commands pc dataPointer input = (pc + 1, dataPointer, input, print dataPointer >> hFlush stdout)

readByteIfStreamOpen :: Word8 -> ByteString -> (Word8, ByteString)
readByteIfStreamOpen currentValue input = fromMaybe (currentValue, input) $ uncons input


forwardJump :: Vector Command -> Int -> Int
forwardJump commands pc =
    findMatch commands (pc + 1) 1
  where
    findMatch commands pc 0 = pc
    findMatch commands pc nestingLevel
        | nestingLevel < 0 = error "Code error: negative nesting level"
        | pc >= Vector.length commands = error "Unmatched '['"
        | commands ! pc == ForwardJumpIfZero = findMatch commands (pc + 1) (nestingLevel + 1)
        | commands ! pc == BackWardJumpIfNonZero = findMatch commands (pc + 1) (nestingLevel - 1)
        | otherwise = findMatch commands (pc + 1) nestingLevel

backwardJump :: Vector Command -> Int -> Int
backwardJump commands pc =
    findMatch commands (pc - 1) 1
  where
    findMatch commands pc 0 = pc
    findMatch commands pc nestingLevel
        | nestingLevel < 0 = error "Code error: negative nesting level"
        | pc < 0 = error "Unmatched ']'"
        | commands ! pc == ForwardJumpIfZero && nestingLevel == 1 = pc
        | commands ! pc == ForwardJumpIfZero = findMatch commands (pc - 1) (nestingLevel - 1)
        | commands ! pc == BackWardJumpIfNonZero = findMatch commands (pc - 1) (nestingLevel + 1)
        | otherwise = findMatch commands (pc - 1) nestingLevel
