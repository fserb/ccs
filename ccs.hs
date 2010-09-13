-- CCS
module Main where

import Control.Monad
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Lazy

import Cache
import Dump

interestingTypes = "(video|audio)"


main = do
  putStrLn "Loading index table..."
  cache <- loadIndexTable indexFile
  
  putStrLn "Loading content types..."
  ctype <- mapM (teeM $ getContentType . Block <=< loadAddr) cache
  
  putStrLn "Filtering and sorting..."
  let interesting = map (\a -> case a of (a, Just b) -> (a, b))
                        $ filter (maybe False (=~ interestingTypes) . snd) ctype
                    
  -- filter ones already dumped with loadDumpMap                   
  return interesting
  
  -- let extensions = mapM (\a -> case a of
  --                               (a, b) -> ((flip constructName b) . Block)
  --                                         `fmap` loadAddr a) interesting
    where teeM f a = f a >>= \x -> return (a, x)