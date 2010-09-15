module Dump
  (
    loadDumpMap,
    dumpBlock,
    dumpPath,
    notOnDumpMap,
    getFilename
  ) where


import Control.Monad
import Data.String.Utils
import Data.Maybe
import Network.URL
import qualified Sound.TagLib as TL
import System.IO
import System.Directory
import System.FilePath
import System.FilePath.Posix
import Data.Digest.Pure.SHA (sha1,Digest)
import Data.Set (Set)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Set as Set
import Text.Printf

import Cache

dumpPath = "dump"
tempFilename = ".tempdump"
audioTypes = [ "audio/mpeg",
               "audio/ogg" ]

getBaseDomain :: Block -> String
getBaseDomain b =
  case getURL b of 
    Right _ -> "unknown"
    Left s -> case importURL s of
                Nothing -> "unknown"
                Just u -> let Absolute a = url_type u
                              sp = split "." (host a)
                          in sp !! (length sp - 2)
                             

makeFileExtension :: Block -> String
makeFileExtension b = 
  case contentType b of
    "audio/mpeg" -> "mp3"
    "video/x-flv" -> "flv"
    _ -> fail "Unknown mime: " ++ (contentType b)


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
    let domain = getBaseDomain b
    let ext = makeFileExtension b
    basename <- nextAvailableName domain s
    return $ basename ++ "." ++ ext
    
    
setAlbum :: Block -> String -> IO ()
setAlbum b s = 
  do
    let d = getBaseDomain b
    tag <- TL.open s >>= maybe (return Nothing) TL.tag 
    case tag of
      Nothing -> return ()  
      Just t -> TL.setAlbum t d
      
    
getFilename :: Block -> String -> IO (Maybe String)
getFilename b s =
  do 
    tag <- TL.open s >>= maybe (return Nothing) TL.tag 
    case tag of
      Nothing -> return Nothing
      Just t -> do
               artist <- TL.artist t
               title <- TL.title t
               let ext = makeFileExtension b
               return $ if (and [(length artist > 0),
                                 (length title > 0)])
                        then Just $ printf "%s - %s.%s" artist title ext  
                        else Nothing


dumpBlock :: Block -> IO ()
dumpBlock b =
  do
    content <- loadAddr $ contentAddress b
    let temp = tempFilename ++ "." ++ makeFileExtension b
    BL8.writeFile temp content
    g <- getFilename b temp
    filename <- case g of
                  Nothing -> makeFilename b dumpPath
                  Just n  -> setAlbum b temp >> return n
    renameFile temp $ dumpPath </> filename
    
      