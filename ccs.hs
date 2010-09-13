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
  
  -- filter ones already dumped with loadDumpMap
  putStrLn "Loading dump map..."
  dumpmap <- loadDumpMap dumpPath
  
  
  putStrLn "Dumping..."    
           -- mapM loadAndDump interesting
  
  -- let extensions = mapM (\a -> case a of
  --                               (a, b) -> ((flip constructName b) . Block)
  --                                         `fmap` loadAddr a) interesting
