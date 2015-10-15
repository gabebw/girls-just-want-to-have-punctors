{-# LANGUAGE OverloadedStrings #-}

-- Solve the problem with regular expressions

module Regex where

import qualified Data.Text as T
import Text.Regex.PCRE ((=~))
import Data.Monoid ((<>))
import Text.Regex (mkRegexWithOpts, subRegex, Regex)
import Common

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

solve :: T.Text -> [T.Text] -> [T.Text] -> [String]
solve originalWord rhymes phrases = map (replaceAnyWith rhymes originalWord) matchingPhrases
    where
        matchingPhrases = phrasesWithAnyRhyme phrases rhymes
