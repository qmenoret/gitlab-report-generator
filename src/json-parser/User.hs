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

-- COMPARATORS
getComparators :: [String] -> [(User -> User -> Ordering)]
getComparators = map getComparator

getMaybeComparator :: String -> Maybe User -> Maybe User -> Ordering
getMaybeComparator _ Nothing Nothing        = EQ
getMaybeComparator _ Nothing _              = LT
getMaybeComparator _ _ Nothing              = GT
getMaybeComparator s (Just u1) (Just u2)    = getComparator s u1 u2

getComparator :: String -> (User -> User -> Ordering)
getComparator "name"        = comparing User.name
getComparator "username"    = comparing User.username
getComparator "id"          = comparing User.id
getComparator "state"       = comparing User.state
getComparator "avatar_url"  = comparing User.avatar_url
getComparator "web_url"     = comparing User.web_url

-- VALUES
getColumnValueMaybe :: String -> Maybe User -> String 
getColumnValueMaybe _ Nothing = ""
getColumnValueMaybe s (Just u) = getColumnValue s u

getColumnValue :: String -> User -> String
getColumnValue "name"        = User.name
getColumnValue "username"    = User.username
getColumnValue "id"          = show . User.id
getColumnValue "state"       = User.state
getColumnValue "avatar_url"  = User.avatar_url
getColumnValue "web_url"     = User.web_url
