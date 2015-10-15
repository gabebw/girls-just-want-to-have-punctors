{-# LANGUAGE OverloadedStrings #-}

-- Easy access to the phrase files

module Phrases where

import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

fileLines :: String -> IO [T.Text]
fileLines n = T.readFile n >>= return . T.splitOn "\n"

phraseFiles :: [String]
phraseFiles = [
    "./phrases/beatles-songs.txt",
    "./phrases/best-selling-books.txt",
    "./phrases/movie-quotes.txt",
    "./phrases/oscar-winning-movies.txt",
    "./phrases/wikipedia-idioms.txt"
    ]
