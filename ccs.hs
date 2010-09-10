-- CCS

import Data.Bits
import Data.Binary.Get
import Data.Word
import Text.Printf
import System.IO
import qualified Data.ByteString.Lazy as BL

data Addr = Addr Int deriving (Show, Eq)
type Filename = String
type FileOffset = (Int, Int, Int)

cachePath = "/home/fserb/.cache/google-chrome/Cache"
indexFile = cachePath ++ "/index"

-- Based on chrome/trunk/src/net/disk_cache/addr.h
getAddr :: Addr -> Either Filename FileOffset
getAddr (Addr a)
  | (a .&. 0x70000000) == 0 = Left $ printf "%06x" (a .&. 0x0FFFFFFF)
  | otherwise               = Right $ ( (a .&. 0x00ff0000) `shiftR` 16, 
                                        (a .&. 0x0000ffff),
                                        ((a .&. 0x03000000) `shiftR` 24) + 1)


getIndexTable :: String -> IO [Addr]
getIndexTable s = do
  h <- openBinaryFile s ReadMode
  bl <- BL.hGetContents h
  ret <- return $ runGet (do 
    return [Addr 0x00])
    bl
  hClose h
  return ret
  
  