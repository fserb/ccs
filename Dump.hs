module Dump
  (
    constructName,
    loadDumpMap,
    dumpAddr
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


constructName :: Block -> String -> (String, String)
constructName b t =
  let ext = case t of
              "audio/mpeg" -> "mp3"
              "video/x-flv" -> "flv"
              _ -> fail "Unknown mime: " ++ t
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
  

makeFilename :: Block -> String -> String -> IO String
makeFilename b t s =
  do
    let (domain, ext) = constructName b t
    basename <- nextAvailableName domain s
    return $ s </> basename ++ "." ++ ext
    

dumpAddr :: Block -> String -> IO ()
dumpAddr b t =
  do
    filename <- makeFilename b t dumpPath
    case getHeaderContentAddr b of 
      Nothing -> return ()
      Just (h_a, c_a) -> do 
        content <- loadAddr c_a
        writeFile filename $ BL8.unpack content
    
      