module Dump
  (
    constructName,
    loadDumpMap
  ) where


import Control.Monad
import Data.String.Utils
import Network.URL
import System.IO
import System.Directory
import System.FilePath
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
