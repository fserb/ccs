-- CCS
module Main where

import Control.Monad
import Data.String.Utils
import Network.URL
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Lazy
import qualified Data.Digest.Pure.SHA (sha1)
import qualified Data.Map as Map

import Cache

interestingTypes = "(video|audio)"

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


main = do
  cache <- loadIndexTable indexFile
  ctype <- mapM (teeM $ getContentType . Block <=< loadAddr) cache
  let interesting = map (\a -> case a of (a, Just b) -> (a, b))
                        $ filter (maybe False (=~ interestingTypes) . snd) ctype

  let extensions = mapM (\a -> case a of
                                 (a, b) -> ((flip constructName b) . Block)
                                           `fmap` loadAddr a) interesting

  return extensions
    where teeM f a = f a >>= \x -> return (a, x)