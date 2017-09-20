module DataPointer where

import Data.Word

data DataPointer = DataPointer {previousCells :: [Word8], currentCell :: Word8, nextCells :: [Word8]}
    deriving (Show)

nextCell :: DataPointer -> DataPointer
nextCell (DataPointer previous current [])     = DataPointer (current : previous) 0 []
nextCell (DataPointer previous current (n:ns)) = DataPointer (current : previous) n ns

prevCell :: DataPointer -> DataPointer
prevCell (DataPointer [] current next) = DataPointer [] 0 (current : next)
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
