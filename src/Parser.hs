{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.ByteString.Lazy as BS

-- Import JSON utils
import Data.Aeson

-- Import actual parser
import qualified ParserConfig as C
import ComplexList
import qualified Task as T

doSort :: C.ParserConfig -> T.Tasks -> T.Tasks
doSort c = complexSort comparators
    where 
        comparators = T.getComparators (C.sortKeys c)

parseTasks :: C.ParserConfig -> IO(T.Tasks)
parseTasks c = do
    issues <- readTasks c
    let Just ts = decode issues :: Maybe T.Tasks
    return ts

readTasks :: C.ParserConfig -> IO(BS.ByteString)
readTasks C.ParserConfig {C.input=C.FromStdin} = BS.getContents
readTasks C.ParserConfig {C.input=C.FromFile fileName} = BS.readFile fileName

doParse :: [String] -> IO(String)
doParse argv = do
    let conf = C.fromArgs argv
    ts <- parseTasks conf
    let sts = doSort conf ts
    return (show sts)
