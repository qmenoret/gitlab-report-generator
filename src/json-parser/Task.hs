{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Task where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

-- Import utilities
import Data.Ord
import Data.List

-- Import SubClasses
import qualified User as U
import qualified Milestone as M

type Tasks = [Task]
data Task = Task            { id                  :: Int
                            , iid                 :: Int
                            , project_id          :: Int
                            , title               :: String
                            , description         :: String
                            , state               :: String
                            , created_at          :: String
                            , updated_at          :: String
                            , labels              :: [String]
                            , milestone           :: M.Milestone
                            , assignee            :: Maybe U.User
                            , subscribed          :: Bool
                            , user_notes_count    :: Int
                            , author              :: U.User
                            , upvotes             :: Int
                            , downvotes           :: Int
                            , due_date            :: Maybe String
                            , confidential        :: Bool
                            , web_url             :: String
                            }
    deriving (Eq, Ord, Generic)
instance FromJSON Task
instance Show Task where
    show x = show $ Task.id x

sortTasks :: (Ord a) => [(Task -> a)] -> Tasks -> Tasks
sortTasks fields ts = sortBy comparator ts
    where
        -- mconcat compareList 
        compareList = map comparing fields
        comparator = mconcat compareList

filterTasks :: [(Task -> Bool)] -> Tasks -> Tasks
filterTasks filters ts = filter globalFilter ts
    where
        globalFilter t = foldl (\acc f -> acc && f t) True filters

