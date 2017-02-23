{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module User where

-- Import JSON utils
import GHC.Generics
import Data.Aeson

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
