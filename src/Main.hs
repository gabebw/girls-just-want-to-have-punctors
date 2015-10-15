{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import qualified Network.Wreq as Wreq (responseBody)
import qualified Data.Text as T
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Regex (solve)
import RhymebrainApi
import Phrases

wordFromArgs :: IO T.Text
wordFromArgs = do
    args <- getArgs
    return $ T.pack $ fromMaybe "heart" $ listToMaybe args

main :: IO ()
main = do
    originalWord <- wordFromArgs
    putStrLn $ T.unpack $ ">> Getting puns for " <> originalWord
    results <- responseBody <$> rhymebrainResults originalWord
    phrases <- concatMapM fileLines phraseFiles
    let puns = Regex.solve originalWord (rhymes results) phrases
    mapM_ putStrLn puns
        where
            responseBody = (^. Wreq.responseBody)
