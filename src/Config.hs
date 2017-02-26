module Config where

import Data.List.Split

data InputType = FromStdin | FromFile String
    deriving (Eq, Show)

data Config = Config    { input     :: InputType
                        , filters   :: [String]
                        , sortKeys  :: [String]
                        }
    deriving (Eq, Show)


defaultConfig = Config  { input     =   FromStdin
                        , filters   =   []
                        , sortKeys  =   ["title"]
                        }

fromArgs :: [String] -> Config
fromArgs = parseArgs defaultConfig

parseArgs :: Config -> [String] -> Config
parseArgs c [] = c
parseArgs c ("--input-file":filename:xs) = parseArgs (c { input      = FromFile filename }) xs
parseArgs c ("--filters":fs:xs)          = parseArgs (c { filters    = splitOn "," fs }         ) xs
parseArgs c ("--sort-keys":ss:xs)        = parseArgs (c { sortKeys   = splitOn "," ss }         ) xs
parseArgs _ (x:xs) = error $ "Wrong arg supplied: " ++ x

