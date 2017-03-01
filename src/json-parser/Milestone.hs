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
                           , description          :: Maybe String
                           , state                :: String
                           , created_at           :: String
                           , updated_at           :: String
                           , due_date             :: Maybe String
                           , start_date           :: Maybe String
                           }
    deriving (Show, Eq, Ord, Generic)
instance FromJSON Milestone

-- COMPARATORS
getComparators :: [String] -> [(Maybe Milestone -> Maybe Milestone -> Ordering)]
getComparators = map getMaybeComparator

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

getMaybeComparator :: String -> Maybe Milestone -> Maybe Milestone -> Ordering
getMaybeComparator _ Nothing Nothing        = EQ
getMaybeComparator _ Nothing _              = LT
getMaybeComparator _ _ Nothing              = GT
getMaybeComparator s (Just m1) (Just m2)    = getComparator s m1 m2

-- VALUES
getColumnValueMaybe :: String -> Maybe Milestone -> String 
getColumnValueMaybe _ Nothing = ""
getColumnValueMaybe s (Just m) = getColumnValue s m

getColumnValue :: String -> Milestone -> String
getColumnValue "id"            = show . Milestone.id
getColumnValue "iid"           = show . Milestone.iid
getColumnValue "project_id"    = show . Milestone.project_id
getColumnValue "title"         = Milestone.title
getColumnValue "description"   = showMaybeString . Milestone.description
getColumnValue "state"         = Milestone.state
getColumnValue "created_at"    = Milestone.created_at
getColumnValue "updated_at"    = Milestone.updated_at
getColumnValue "due_date"      = showMaybeString . Milestone.due_date
getColumnValue "start_date"    = showMaybeString . Milestone.start_date

-- Filters
isActive :: Milestone -> Bool
isActive = (== "active") . Milestone.state

isInactive :: Milestone -> Bool
isInactive = (== "inactive") . Milestone.state

-- Utils
showMaybeString :: Maybe String -> String
showMaybeString Nothing = ""
showMaybeString (Just x) = x
