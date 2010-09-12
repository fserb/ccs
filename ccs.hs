-- CCS
module Main where

import Control.Monad
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Lazy

import Cache
import Dump

interestingTypes = "(video|audio)"


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