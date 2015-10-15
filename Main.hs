{-# LANGUAGE OverloadedStrings #-}

import Control.Lens ((^.))
import Network.Wreq (responseBody)
import qualified Data.Text as T
import Data.Monoid ((<>))
import System.Environment (getArgs)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Regex
import Common (rhymebrainResults)

wordFromArgs :: IO T.Text
wordFromArgs = do
    args <- getArgs
    return $ T.pack $ fromMaybe "heart" $ listToMaybe args

main = do
    originalWord <- wordFromArgs
    print $ "Getting puns for " <> originalWord
    r <- rhymebrainResults originalWord
    let rhymebrainResults = r ^. responseBody
    puns <- Regex.solve originalWord rhymebrainResults
    mapM_ putStrLn puns
