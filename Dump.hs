module Dump
  (
    constructName,
    loadDumpMap,
    dumpBlock,
    dumpPath,
    notOnDumpMap
  ) where


import Control.Monad
import Data.String.Utils
import Data.Maybe
import Network.URL
import System.IO
import System.Directory
import System.FilePath
import System.FilePath.Posix
import Data.Digest.Pure.SHA (sha1,Digest)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Set as Set

import Cache

dumpPath = "dump"

getBaseDomain :: String -> String
getBaseDomain s =
  case importURL s of
    Nothing -> "unknown"
    Just u -> let Absolute a = url_type u
                  sp = split "." (host a)
              in sp !! (length sp - 2)


constructName :: Block -> (String, String)
constructName b =
  let ext = case contentType b of
              "audio/mpeg" -> "mp3"
              "video/x-flv" -> "flv"
              _ -> fail "Unknown mime: " ++ (contentType b)
      domain = case getURL b of
                 Left s -> getBaseDomain s
                 Right a -> "unknown"
  in (domain, ext)


loadDumpMap :: String -> IO (Set Digest)
loadDumpMap s =
  do
    c <- getDirectoryContents s
    f <- filterM (doesFileExist . (s </>)) c
    h <- mapM (getFile >=> (return . sha1)) f
    return $ Set.fromList h
  where getFile = flip openBinaryFile ReadMode . (s </>)
                  >=> BL.hGetContents


notOnDumpMap :: Set Digest -> Block -> IO Bool
notOnDumpMap s b = 
  do
    c <- loadAddr $ contentAddress b
    return $ Set.notMember (sha1 c) s
  

nextAvailableName :: String -> String -> IO String
nextAvailableName n s =
  do
    c <- getDirectoryContents s
    f <- filterM (doesFileExist . (s </>)) c
    let names = (map dropExtension f)
    let numbers = mapMaybe filterNamesOut names
    let next = 1 + foldl max 0 numbers
    return $ n ++ "-" ++ show next
  where filterNamesOut x = if startswith (n ++ "-") x
                           then Just $ read $ drop (length n + 1) x 
                           else Nothing
  

makeFilename :: Block -> String -> IO String
makeFilename b s =
  do
    let (domain, ext) = constructName b
    basename <- nextAvailableName domain s
    return $ s </> basename ++ "." ++ ext
    

dumpBlock :: Block -> IO ()
dumpBlock b =
  do
    filename <- makeFilename b dumpPath
    content <- loadAddr $ contentAddress b
    BL8.writeFile filename content
    
      