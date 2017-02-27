{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib
    ( parserEntry
    ) where

import System.Environment   

-- Import SubClasses
import qualified Task as T
import qualified Parser as P

parserEntry :: IO()
parserEntry = do
    argv <- getArgs
    tasks <- P.doParse argv
    putStrLn tasks
