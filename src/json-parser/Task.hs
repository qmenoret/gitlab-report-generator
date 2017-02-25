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

sortTasks :: [(Task -> Task -> Ordering)] -> Tasks -> Tasks
sortTasks fields ts = sortBy comparator ts
    where
        -- mconcat compareList 
        comparator = mconcat fields

filterTasks :: [(Task -> Bool)] -> Tasks -> Tasks
filterTasks filters ts = filter globalFilter ts
    where
        globalFilter t = foldl (\acc f -> acc && f t) True filters

getComparators :: [String] -> [(Task -> Task -> Ordering)]
getComparators = foldl (\acc x -> (getComparator x) : acc) []

getComparator :: String -> (Task -> Task -> Ordering)
getComparator "id"                  = comparing Task.id
getComparator "iid"                 = comparing Task.iid
getComparator "project_id"          = comparing Task.project_id
getComparator "title"               = comparing Task.title
getComparator "description"         = comparing Task.description
getComparator "state"               = comparing Task.state
getComparator "created_at"          = comparing Task.created_at
getComparator "updated_at"          = comparing Task.updated_at
getComparator "labels"              = comparing Task.labels
getComparator "milestone"           = comparing Task.milestone
getComparator "assignee"            = comparing Task.assignee
getComparator "subscribed"          = comparing Task.subscribed
getComparator "user_notes_count"    = comparing Task.user_notes_count
getComparator "author"              = comparing Task.author
getComparator "upvotes"             = comparing Task.upvotes
getComparator "downvotes"           = comparing Task.downvotes
getComparator "due_date"            = comparing Task.due_date
getComparator "confidential"        = comparing Task.confidential
getComparator "web_url"             = comparing Task.web_url
getComparator xs 
    | m1 == "milestone" = comparing Task.milestone
    | m1 == "assignee"  = comparing Task.assignee
    | m1 == "author"    = comparing Task.author
    where
        (m1,m2) = span ('.' /=) xs

