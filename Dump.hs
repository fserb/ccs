module Dump
  (
    loadDumpMap,
    dumpBlock,
    dumpPath,
    dumpMap,
    makeDumpMap,
    notOnDumpMap,
  ) where


import Control.Monad
import Data.String.Utils
import Data.Char
import Data.Bits
import Data.Maybe
import System.IO
import System.Directory
import System.FilePath
import System.FilePath.Posix
import Data.Digest.Pure.SHA (sha1,bytestringDigest,showDigest)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Set as Set
import Text.Printf
import Text.Read

import Cache
import Naming

dumpPath = "dump"
dumpSHA1Map = ".dumpsha1"
tempFilename = ".tempdump"
audioTypes = [ "audio/mpeg",
               "audio/ogg" ]

newtype Digest = Digest BL8.ByteString deriving (Eq,Ord)
instance Show Digest where
  show (Digest bs) = foldr paddedShowHex [] (BL8.unpack bs)
    where
      paddedShowHex x xs = intToDigit (ord x `shiftR` 4)
                           : intToDigit (ord x .&. 0xf)
                           : xs


loadDumpMapFromDir :: String -> IO (Set Digest)
loadDumpMapFromDir s =
  do
    c <- getDirectoryContents s
    f <- filterM (doesFileExist . (s </>)) c
    h <- mapM (getFile >=> (return . sha1)) f
    return $ Set.fromList $ map (Digest . bytestringDigest) h
  where getFile = flip openBinaryFile ReadMode . (s </>)
                  >=> BL.hGetContents


loadDumpMapFromFile :: String -> IO (Set Digest)
loadDumpMapFromFile f =
  do
    e <- doesFileExist f
    if e
     then readFile f >>= return . Set.fromList .  map readDigest . lines
     else return Set.empty
  where readDigest "" = Digest BL8.empty
        readDigest (x:y:xs) = Digest $
                              BL8.cons (chr $ compact x y) (get $ readDigest xs)
          where compact x y = (digitToInt x `shiftL` 4) + (digitToInt y)
                get (Digest b) = b


dumpMap :: Set Digest -> IO ()
dumpMap s =
  do
    h <- openFile (dumpPath </> dumpSHA1Map) WriteMode
    mapM (hPutStrLn h . show) (Set.toList s)
    hClose h


makeDumpMap :: [Block] -> Set Digest -> IO (Set Digest)
makeDumpMap b s =
  do
    dg <- mapM ((makeDigest `fmap`) . loadData) b
    return $ Set.union s (Set.fromList dg)
  where makeDigest = Digest . bytestringDigest . sha1
        loadData = loadAddr . contentAddress


loadDumpMap :: String -> IO (Set Digest)
loadDumpMap s =
  do
    f <- loadDumpMapFromFile $ s </> dumpSHA1Map
    if f == Set.empty
     then loadDumpMapFromDir s
     else return f


notOnDumpMap :: Set Digest -> Block -> IO Bool
notOnDumpMap s b =
  do
    c <- loadAddr $ contentAddress b
    return $ Set.notMember (Digest . bytestringDigest $ sha1 c) s


dumpBlock :: Block -> IO ()
dumpBlock b =
  do
    content <- loadAddr $ contentAddress b
    let temp = tempFilename ++ "." ++ makeFileExtension b
    BL8.writeFile temp content
    filename <- blockFilename dumpPath b temp
    renameFile temp $ dumpPath </> filename

