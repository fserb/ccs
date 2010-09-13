-- CCS
module Main where

import Control.Monad
import Data.Maybe
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString.Lazy

import Cache
import Dump

interestingTypes = "(video|audio)"


main = do
  putStrLn "Loading index table..."
  cache <- loadIndexTable indexFile
  
  putStrLn "Loading cache..."
  block <- mapM createBlock cache
  
  putStrLn "Filtering and sorting..."
  let interesting = filter ((=~ interestingTypes) . contentType) 
                    $ catMaybes block
  
  putStrLn "Loading dump map..."
  dumpmap <- loadDumpMap dumpPath
  
  putStrLn "Filtering dump map..."
  final <- filterM (notOnDumpMap dumpmap) interesting

  putStrLn ("Dumping " ++ show (length final) ++ " files...")
  mapM dumpBlock final

           -- mapM loadAndDump interesting
  
  -- let extensions = mapM (\a -> case a of
  --                               (a, b) -> ((flip constructName b) . Block)
  --                                         `fmap` loadAddr a) interesting
