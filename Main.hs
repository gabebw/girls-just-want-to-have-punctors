{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Lens
import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Aeson
import GHC.Generics
import Data.Function (on)
import Data.List (maximumBy)
import Control.Monad (liftM)
import Text.Regex.PCRE ((=~))
import Data.Monoid ((<>))
import Text.Regex (mkRegexWithOpts, subRegex, Regex)
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

data RhymebrainResult = RhymebrainResult { score :: Int, word :: T.Text }
    deriving (Generic, FromJSON, Show, Eq)

instance Ord RhymebrainResult where
    compare = compare `on` score

-- Convenience function
concatMapM f x = liftM concat $ mapM f x

rhymebrainOptions :: T.Text -> Options
rhymebrainOptions word = defaults &
    param "function" .~ ["getRhymes"] &
    param "maxResults" .~ ["0"] &
    param "lang" .~ ["en"] &
    param "word" .~ [word]

rhymebrainHost :: String
rhymebrainHost = "http://rhymebrain.com/talk"

rhymebrainResults :: T.Text -> IO (Response [RhymebrainResult])
rhymebrainResults word = asJSON =<< getWith (rhymebrainOptions word) rhymebrainHost

resultsWithScore :: Int -> [RhymebrainResult] -> [RhymebrainResult]
resultsWithScore s = filter (\result -> score result == s)

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

phrasesWithAnyRhyme :: [T.Text] -> [T.Text] -> [T.Text]
phrasesWithAnyRhyme phrases rhymes = filter (containsAnyOf rhymes) phrases

-- Does the given phrase contain any of the given rhymes?
containsAnyOf :: [T.Text] -> T.Text -> Bool
containsAnyOf rhymes phrase = T.unpack phrase =~ anyPCRE rhymes

replaceAnyWith :: [T.Text] -> T.Text -> T.Text -> String
replaceAnyWith rhymes originalWord phrase = subRegex (anyPosix rhymes) (T.unpack phrase) (T.unpack originalWord)

anyPosix :: [T.Text] -> Text.Regex.Regex
anyPosix rhymes = mkRegexWithOpts (withPosixWordBoundaries $ T.intercalate "|" rhymes) True False
    where
        withPosixWordBoundaries t = T.unpack $ "[[:<:]](" <> t <> ")[[:>:]]"

anyPCRE :: [T.Text] -> String
anyPCRE rhymes = T.unpack $ withPCREWordBoundaries $ T.intercalate "|" rhymes
    where
        withPCREWordBoundaries t = "(?i)\\b(" <> t <> ")\\b"

wordFromArgs :: IO T.Text
wordFromArgs = do
    args <- getArgs
    return $ T.pack $ fromMaybe "heart" $ listToMaybe args

main = do
    originalWord <- wordFromArgs
    print $ "Getting puns for " <> originalWord
    r <- rhymebrainResults originalWord
    let rhymebrainResults = r ^. responseBody
    let highestScoringResults = resultsWithScore (score $ maximum rhymebrainResults) rhymebrainResults
    let rhymes = map word highestScoringResults
    phrases <- concatMapM fileLines phraseFiles
    let matchingPhrases = phrasesWithAnyRhyme phrases rhymes
    let puns = map (replaceAnyWith rhymes originalWord) matchingPhrases
    mapM_ putStrLn puns
