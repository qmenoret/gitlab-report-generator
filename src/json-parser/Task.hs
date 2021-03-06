{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Task where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

-- Import utilities
import Data.Ord
import Data.Maybe

-- Import SubClasses
import qualified User as U
import qualified Milestone as M

type Tasks = [Task]
data Task = Task            { id                  :: Int
                            , iid                 :: Int
                            , project_id          :: Int
                            , title               :: String
                            , description         :: Maybe String
                            , state               :: String
                            , created_at          :: String
                            , updated_at          :: String
                            , labels              :: [String]
                            , milestone           :: Maybe M.Milestone
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
    show x = show $ Task.title x

getComparators :: [String] -> [(Task -> Task -> Ordering)]
getComparators = map getComparator

getComparator :: String -> (Task -> Task -> Ordering)
getComparator ('#':xs)              = flip $ getComparator xs
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
    | m1 == "author"    = (\t1 t2 -> (U.getComparator m2)   (Task.author t1)      (Task.author t2))
    | m1 == "milestone" = (\t1 t2 -> (M.getMaybeComparator m2)   (Task.milestone t1)   (Task.milestone t2))
    | m1 == "assignee"  = (\t1 t2 -> (U.getMaybeComparator m2)   (Task.assignee t1)    (Task.assignee t2))
    where
        (m1,dot:m2) = span ('.' /=) xs

getColumnValue :: String -> Task -> String
getColumnValue "id"                  = show . Task.id
getColumnValue "iid"                 = show . Task.iid
getColumnValue "project_id"          = show . Task.project_id
getColumnValue "title"               = Task.title
getColumnValue "description"         = showMaybeString . Task.description
getColumnValue "state"               = Task.state
getColumnValue "created_at"          = Task.created_at
getColumnValue "updated_at"          = Task.updated_at
getColumnValue "labels"              = show . Task.labels
getColumnValue "milestone"           = showMaybe . Task.milestone
getColumnValue "assignee"            = showMaybe . Task.assignee
getColumnValue "subscribed"          = show . Task.subscribed
getColumnValue "user_notes_count"    = show . Task.user_notes_count
getColumnValue "author"              = show . Task.author
getColumnValue "upvotes"             = show . Task.upvotes
getColumnValue "downvotes"           = show . Task.downvotes
getColumnValue "due_date"            = show . Task.due_date
getColumnValue "confidential"        = show . Task.confidential
getColumnValue "web_url"             = Task.web_url
getColumnValue xs 
    | m1 == "author"    = U.getColumnValue m2 . Task.author
    | m1 == "milestone" = M.getColumnValueMaybe m2 . Task.milestone
    | m1 == "assignee"  = U.getColumnValueMaybe m2 . Task.assignee
    where
        (m1,dot:m2) = span ('.' /=) xs

-- Filters
getFilters :: [String] -> [(Task -> Bool)]
getFilters = map getFilter

getFilter :: String -> (Task -> Bool)
getFilter ('#':xs)        = (not) . (getFilter xs)
getFilter "open"          = isOpen
getFilter "closed"        = isClosed
getFilter "assigned"      = isAssigned
getFilter "unassigned"    = isUnassigned
getFilter "active"        = inActiveMilestone
getFilter "inactive"      = inInactiveMilestone
getFilter xs | '=' `elem` xs = (== e2) . getColumnValue e1
             | '~' `elem` xs = (/= e2) . getColumnValue e1
             | '>' `elem` xs = (>  e2) . getColumnValue e1
             | '<' `elem` xs = (<  e2) . getColumnValue e1
    where
        (e1,op:e2) = span (`notElem` ['~','=','>','<']) xs 

-- List of predefined filters
isOpen :: Task -> Bool
isOpen = (== "opened") . Task.state

isClosed :: Task -> Bool
isClosed = (== "closed") . Task.state

isAssigned :: Task -> Bool
isAssigned = not . isUnassigned

isUnassigned :: Task -> Bool
isUnassigned = isNothing . Task.assignee

inActiveMilestone :: Task -> Bool
inActiveMilestone Task{milestone=Nothing} = False
inActiveMilestone t = (M.isActive . fromJust . Task.milestone) t

inInactiveMilestone :: Task -> Bool
inInactiveMilestone Task{milestone=Nothing} = True
inInactiveMilestone t = (M.isInactive . fromJust . Task.milestone) t

-- Utils
showMaybe :: (Show a) => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just x) = show x

showMaybeString :: Maybe String -> String
showMaybeString Nothing = ""
showMaybeString (Just x) = x
