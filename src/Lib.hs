{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib
    ( sortTasks
    ) where

-- Import JSON utils
import Data.Aeson
import qualified Data.ByteString.Lazy as BS

-- Import utilities
import Data.Ord
import Data.List

-- Import SubClasses
import qualified Task as T

sortTasks :: (Ord a) => [(T.Task -> a)] -> T.Tasks -> T.Tasks
sortTasks fields ts = sortBy comparator ts
    where
        -- mconcat compareList 
        compareList = map comparing fields
        comparator = mconcat compareList

