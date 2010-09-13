module Cache
  ( Addr(Addr),
    Block(Block),
    loadIndexTable,
    indexFile,
    getContentType,
    getHeaderContentAddr,
    getURL,
    loadAddr,
  ) where

import Control.Monad
import Data.Binary.Get
import Data.Bits
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Word
import Magic
import System.FilePath
import System.IO
import Text.Printf
import Text.Regex.Base
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.ByteString.Lazy as BL
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
  | otherwise =
    let file_number = (a .&. 0x00ff0000) `shiftR` 16
        start_block = (a .&. 0x0000ffff)
        num_block   = ((a .&. 0x03000000) `shiftR` 24) + 1
        file_type   = (a .&. 0x70000000) `shiftR` 28
        block_size  = [0, 36, 256, 1024, 4096] !! file_type
        offset_start = 8192 + block_size*start_block
        size = num_block*block_size
    in Right (file_number, offset_start, size)


getIndexTable :: BL.ByteString -> [Addr]
getIndexTable =
  runGet $ do
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
newtype Data  = Data BL.ByteString

getURL :: Block -> Either String Addr
getURL (Block b) =
  flip runGet b $ do
    skip (4*8)
    key_len <- getWord32le
    cache_key <- ((Addr . fromIntegral) `fmap` getWord32le)
    if cache_key /= Addr 0
      then return $ Right cache_key
      else skip (4*14)
             >> ((Left . BL8.unpack) `fmap`
                 getLazyByteString (fromIntegral key_len))


getDataAddr :: Block -> [Addr]
getDataAddr (Block b) =
  flip runGet b $ do
    skip(4*10)
    size <- replicateM 4 (fromIntegral `fmap` getWord32le)
    addr <- replicateM 4 ((Addr . fromIntegral) `fmap`
                          getWord32le)
    return $ map fst $ filter ((/= 0) . snd) $ zip addr size

getHeaderContentAddr :: Block -> Maybe (Addr, Addr)
getHeaderContentAddr b =
  case getDataAddr b of
    [] -> Nothing
    [_] -> Nothing
    [header, content] -> Just (header, content)
    d -> fail $ "Weird number of data: " ++ show d


getContentTypeFromHeader :: Data -> Maybe String
getContentTypeFromHeader (Data bl) =
  let s = replace "\NUL" "\n" $ BL8.unpack bl
      ct = s =~ "^Content-Type: ([^; \n]*)" :: [[String]]
  in case ct of
       [_,s]:xs -> Just s
       [] -> Nothing

getContentTypeFromContent :: Data -> IO (Maybe String)
getContentTypeFromContent (Data bl) =
  do
    m <- magicOpen [MagicMime]
    magicLoadDefault m
    t <- magicString m $ BL8.unpack bl
    case t of
      "" -> return Nothing
      s -> return $ Just $ split ";" s !! 0


getContentType :: Block -> IO (Maybe String)
getContentType b =
  case getHeaderContentAddr b of
    Just (header_a, content_a) ->
      do header <- Data `fmap` loadAddr header_a
         case getContentTypeFromHeader header of
           Nothing -> loadContentAndGetType
           Just "application/octet-stream" -> loadContentAndGetType
           c -> return c
         where loadContentAndGetType = Data `fmap`
                 loadAddr content_a >>= getContentTypeFromContent
    Nothing -> return Nothing


loadAddr :: Addr -> IO BL.ByteString
loadAddr a =
  case getAddr a of
    Left filename -> openBinaryFile
                     (cachePath </> filename) ReadMode
                    >>= BL.hGetContents
    Right (filenumber, start, size) ->
      let filename = cachePath </> printf "data_%01d" filenumber
      in do
        h <- openBinaryFile filename ReadMode
        hSeek h AbsoluteSeek $ toInteger start
        BL.hGet h size


loadIndexTable :: String -> IO [Addr]
loadIndexTable s = do
  h <- openBinaryFile s ReadMode
  bl <- BL.hGetContents h
  return $ getIndexTable bl
