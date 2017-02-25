{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module User where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

-- Import Utililities
import Data.Ord

-- TODO
-- data UserState = Active | Inactive
--     deriving (Show, Generic)
-- instance FromJSON UserState
type UserState = String

type Users = [User]
data User = User            { name                  :: String
                            , username              :: String
                            , id                    :: Int
                            , state                 :: UserState
                            , avatar_url            :: String
                            , web_url               :: String
                            }
    deriving (Show, Eq, Ord, Generic)
instance FromJSON User

getComparators :: [String] -> [(User -> User -> Ordering)]
getComparators = map getComparator

getComparator :: String -> (User -> User -> Ordering)
getComparator "name"        = comparing User.name
getComparator "username"    = comparing User.username
getComparator "id"          = comparing User.id
getComparator "state"       = comparing User.state
getComparator "avatar_url"  = comparing User.avatar_url
getComparator "web_url"     = comparing User.web_url
