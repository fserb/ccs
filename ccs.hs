-- CCS

import Data.Bits
import Text.Printf

data Addr = Addr Int deriving (Show, Eq)
type Filename = String
type FileOffset = (Int, Int, Int)

getAddr :: Addr -> Either Filename FileOffset
getAddr (Addr a)
  | (a .&. 0x70000000) == 0 = Left $ printf "%06x" (a .&. 0x0FFFFFFF)
  | otherwise = Right $ ( (a .&. 0x00ff0000) `shiftR` 16, 
                          (a .&. 0x0000ffff),
                          ((a .&. 0x03000000) `shiftR` 24) + 1)


