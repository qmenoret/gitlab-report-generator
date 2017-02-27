module ParserConfig where

import Data.List.Split

data InputType = FromStdin | FromFile String
    deriving (Eq, Show)

data ParserConfig = ParserConfig    { input     :: InputType
                                    , filters   :: [String]
                                    , sortKeys  :: [String]
                                    }
    deriving (Eq, Show)


defaultParserConfig = ParserConfig  { input     =   FromStdin
                                    , filters   =   []
                                    , sortKeys  =   ["title"]
                                    }

fromArgs :: [String] -> ParserConfig
fromArgs = parseArgs defaultParserConfig

parseArgs :: ParserConfig -> [String] -> ParserConfig
parseArgs c [] = c
parseArgs c ("--input-file":filename:xs) = parseArgs (c { input      = FromFile filename }) xs
parseArgs c ("--filters":fs:xs)          = parseArgs (c { filters    = splitOn "," fs }         ) xs
parseArgs c ("--sort-keys":ss:xs)        = parseArgs (c { sortKeys   = splitOn "," ss }         ) xs
parseArgs _ (x:xs) = error $ "Wrong arg supplied: " ++ x
