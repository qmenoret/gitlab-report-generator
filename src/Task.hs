{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Task where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

-- Import SubClasses
import qualified User as U
import qualified Milestone as M

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
    deriving (Show, Generic)
instance FromJSON Task
