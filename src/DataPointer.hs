{- |
Module      :  DataPointer
Description :  Data type representing the byte array containing the internal
state of the interpreter
Copyright   :  (c) Peter Uhlenbruck 2017
License     :  MIT Licence
-}
module DataPointer where

import Data.Word

data DataPointer = DataPointer {previousCells :: [Word8], currentCell :: Word8, nextCells :: [Word8]}
    deriving (Show)

nextCell :: DataPointer -> DataPointer
nextCell dp@(DataPointer [] 0 [])              = dp
nextCell (DataPointer previous current [])     = DataPointer (current : previous) 0 []
nextCell (DataPointer [] 0 (n:ns))             = DataPointer [] n ns
nextCell (DataPointer previous current (n:ns)) = DataPointer (current : previous) n ns

prevCell :: DataPointer -> DataPointer
prevCell dp@(DataPointer [] 0 [])          = dp
prevCell (DataPointer [] current next)     = DataPointer [] 0 (current : next)
prevCell (DataPointer (p:ps) 0 [])         = DataPointer ps p []
prevCell (DataPointer (p:ps) current next) = DataPointer ps p (current : next)

incrementCurrent :: DataPointer -> DataPointer
incrementCurrent (DataPointer previous current next) = DataPointer previous (current + 1) next

decrementCurrent :: DataPointer -> DataPointer
decrementCurrent (DataPointer previous current next) = DataPointer previous (current - 1) next


setCurrent :: DataPointer -> Word8 -> DataPointer
setCurrent dataPointer newValue = dataPointer {currentCell = newValue}

zero :: DataPointer -> Bool
zero (DataPointer _ 0 _) = True
zero _                   = False
