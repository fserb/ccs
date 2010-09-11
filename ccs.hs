-- CCS

import Control.Monad
import Data.Bits
import Data.String.Utils
import Data.Binary.Get
import Data.Word
import Maybe
import Text.Printf
import System.IO
import System.EasyFile
import Text.Regex.Base
import Text.Regex.Posix
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 as BL8

cachePath = "/home/fserb/.cache/google-chrome/Cache"
indexFile = cachePath ++ "/index"

newtype Addr = Addr Int deriving (Show, Eq)
type Filename = String
type FileOffset = (Int, Int, Int) -- file, offset start, size

-- Based on chrome/trunk/src/net/disk_cache/addr.h
getAddr :: Addr -> Either Filename FileOffset
getAddr (Addr a)
  | (a .&. 0x70000000) == 0 = Left $ printf "f_%06x" (a .&. 0x0FFFFFFF)
  | otherwise               = let file_number = (a .&. 0x00ff0000) `shiftR` 16
                                  start_block = (a .&. 0x0000ffff)
                                  num_block   = ((a .&. 0x03000000) `shiftR` 24) + 1
                                  file_type   = (a .&. 0x70000000) `shiftR` 28
                                  block_size  = [0, 36, 256, 1024, 4096] !! file_type
                                  offset_start = 8192 + block_size*start_block
                                  size = num_block*block_size
                              in Right (file_number, offset_start, size)


getIndexTable :: BL.ByteString -> [Addr]
getIndexTable = runGet $ do
                  magic <- getWord32le
                  when (magic /= 0xC103CAC3) (fail "No magic")
                  skip (4*6)
                  table_len <- getWord32le
                  -- skip 3 words from header, 52 pads and 28 from LRU
                  skip (4*3 + 4*52 + 4*28)
                  -- load the address table here
                  all <- replicateM (fromIntegral table_len)
                                    ((Addr . fromIntegral) `fmap` getWord32le)
                  return $ filter (/= Addr 0) all


newtype Block = Block BL.ByteString

getURL :: Block -> Either String Addr
getURL (Block b) = flip runGet b $ do
                     skip (4*8)
                     key_len <- getWord32le
                     cache_key <- ((Addr . fromIntegral) `fmap` getWord32le)
                     if (cache_key /= Addr 0)
                       then return $ Right cache_key
                       else skip (4*14) 
                              >> ((Left . BL8.unpack) `fmap` 
                                  getLazyByteString (fromIntegral key_len))


getDataAddr :: Block -> [Addr]
getDataAddr (Block b) = flip runGet b $ do
                          skip(4*10)
                          size <- replicateM 4 (fromIntegral `fmap` getWord32le)
                          addr <- replicateM 4 ((Addr . fromIntegral) `fmap` getWord32le)
                          return $ map fst $ filter ((/= 0) . snd) $ zip addr size


getContentTypeFromHeader :: BL.ByteString -> Maybe String
getContentTypeFromHeader bl = let s = replace "\NUL" "\n" $ BLU.toString bl
                                  ct = s =~ "^Content-Type: (.*)$" :: [[String]]
                              in case ct of 
                                   [[_,s]] -> Just s
                                   [] -> Nothing


getContentTypeFromContent :: BL.ByteString -> Maybe String
getContentTypeFromContent bl = Nothing


-- Impure functions from hell


getContentType:: Block -> IO (Maybe String)
getContentType b = let d = getDataAddr b 
                   in case d of
                        [header_a, content_a] -> do
                               header <- loadAddr header_a
                               let ct = getContentTypeFromHeader header
                                 in case ct of
                                      Just c -> return $ Just c
                                      Nothing -> loadAddr content_a
                                                 >>= return . getContentTypeFromContent 
                        _ -> fail $ "Weird number of data: " ++ show d
                                       

loadAddr :: Addr -> IO BL.ByteString
loadAddr a = let addr = getAddr a 
             in case addr of
                  Left filename -> openBinaryFile (joinPath [cachePath, filename]) ReadMode 
                                   >>= BL.hGetContents
                  Right (filenumber, start, size) -> 
                    let filename = joinPath [ cachePath, printf "data_%01d" filenumber ]
                    in do
                      h <- openBinaryFile filename ReadMode 
                      hSeek h AbsoluteSeek $ toInteger start
                      BL.hGet h size


loadIndexTable :: String -> IO [Addr]
loadIndexTable s = do
  h <- openBinaryFile s ReadMode
  bl <- BL.hGetContents h
  return $ getIndexTable bl


main = do
  cache <- loadIndexTable indexFile
  return ""

