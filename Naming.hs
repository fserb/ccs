module Naming
  (
   blockFilename,
   makeFileExtension
  ) where

import Control.Monad
import Network.URL
import Data.String.Utils
import System.Directory
import System.FilePath
import System.FilePath.Posix
import qualified Sound.TagLib as TL
import Data.Maybe
import Text.Printf

import Cache

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
    tagfile <- TL.open s
    tag <- maybe (return Nothing) TL.tag tagfile
    case tag of
      Nothing -> return ()
      Just t -> do
               TL.setAlbum t "CCS" 
               maybe (return 0) TL.save tagfile 
               return ()


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


blockFilename :: String -> Block -> String -> IO String
blockFilename path b temp =
  do
    g <- getFilename b temp
    case g of
      Nothing -> makeFilename b path
      Just n  -> setAlbum b temp >> return n
