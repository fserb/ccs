module Dump
  (
    constructName
  ) where

import Data.String.Utils
import Network.URL
import qualified Data.Digest.Pure.SHA (sha1)
import qualified Data.Map as Map

import Cache

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

