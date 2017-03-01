{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib
    ( parserEntry
    ) where

import System.Environment   

-- Import SubClasses
import qualified Task as T
import qualified Parser as P
import Markdown

parserEntry :: IO()
parserEntry = do
    argv <- getArgs
    lines <- P.doParse argv
    putStrLn $ table lines
