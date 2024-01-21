module AoC where


readLines :: String -> IO [String]
readLines p = lines <$> readFile p
