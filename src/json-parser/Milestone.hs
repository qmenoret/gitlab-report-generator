{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Milestone where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

-- Import Utililities
import Data.Ord

type Milestones = [Milestone]
data Milestone = Milestone { id                   :: Int
                           , iid                  :: Int
                           , project_id           :: Int
                           , title                :: String
                           , description          :: String
                           , state                :: String
                           , created_at           :: String
                           , updated_at           :: String
                           , due_date             :: Maybe String
                           , start_date           :: Maybe String
                           }
    deriving (Show, Eq, Ord, Generic)
instance FromJSON Milestone

getComparators :: [String] -> [(Milestone -> Milestone -> Ordering)]
getComparators = map getComparator

getComparator :: String -> (Milestone -> Milestone -> Ordering)
getComparator "id"              = comparing Milestone.id
getComparator "iid"             = comparing Milestone.iid
getComparator "project_id"      = comparing Milestone.project_id
getComparator "title"           = comparing Milestone.title
getComparator "description"     = comparing Milestone.description
getComparator "state"           = comparing Milestone.state
getComparator "created_at"      = comparing Milestone.created_at
getComparator "updated_at"      = comparing Milestone.updated_at
getComparator "due_date"        = comparing Milestone.due_date
getComparator "start_date"      = comparing Milestone.start_date


-- List of predefined filters
isActive :: Milestone -> Bool
isActive = (== "active") . Milestone.state

isInactive :: Milestone -> Bool
isInactive = (== "inactive") . Milestone.state


