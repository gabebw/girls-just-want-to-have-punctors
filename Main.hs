{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Network.Wreq (responseBody)
import qualified Data.Text as T
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Regex (solve)
import Common

wordFromArgs :: IO T.Text
wordFromArgs = do
    args <- getArgs
    return $ T.pack $ fromMaybe "heart" $ listToMaybe args

main :: IO ()
main = do
    originalWord <- wordFromArgs
    print $ "Getting puns for " <> originalWord
    r <- rhymebrainResults originalWord
    let results = r ^. responseBody
    let highestScoringResults = resultsWithScore (score $ maximum results) results
    let rhymes = map word highestScoringResults
    phrases <- concatMapM fileLines phraseFiles
    let puns = Regex.solve originalWord rhymes phrases
    mapM_ putStrLn puns
