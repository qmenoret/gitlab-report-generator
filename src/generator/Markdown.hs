
module Markdown where


table :: [[String]] -> String
table (headers:rows) = twoRows ++ foldl (\acc x -> acc ++ (row x)) "" rows
    where
        twoRows = (row headers) ++ (row . replicate (length headers)) "---"
        row content = (foldl (\acc x -> acc ++ cell x) "" content) ++ "|\n"
        cell ""   = "| "
        cell word = '|':word
