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
                            , description         :: String
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
    -- FIXME Error on nothing
    | m1 == "milestone" = (\t1 t2 -> (M.getComparator m2)   (fromJust (Task.milestone t1))   (fromJust (Task.milestone t2)))
    | m1 == "author"    = (\t1 t2 -> (U.getComparator m2)   (Task.author t1)      (Task.author t2))
    -- TODO (maybe)
    -- | m1 == "assignee"  = (\t1 t2 -> (U.getComparator m2)   (Task.assignee t1)    (Task.assignee t2))
    where
        (m1,dot:m2) = span ('.' /=) xs

getFilters :: [String] -> [(Task -> Bool)]
getFilters [] = []
getFilters ("open":xs)          = isOpen                : getFilters xs
getFilters ("closed":xs)        = isClosed              : getFilters xs
getFilters ("assigned":xs)      = isAssigned            : getFilters xs
getFilters ("unassigned":xs)    = isUnassigned          : getFilters xs
getFilters ("active":xs)        = inActiveMilestone     : getFilters xs
getFilters ("inactive":xs)      = inInactiveMilestone   : getFilters xs

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


