{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Milestone where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

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
