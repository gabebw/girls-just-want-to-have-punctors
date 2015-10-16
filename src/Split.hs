{-# LANGUAGE OverloadedStrings #-}

-- We lowercase everything to make comparing easier, and don't un-lowercase
-- because it's tricky (if it was "Heart", then we'd need to ensure the pun is
-- "Cart", not "cart").

module Split where

import qualified Data.Text as T
import Data.Monoid ((<>))
import Pun

-- Does the haystack contain any of the needles, with word boundaries?
containsAnyWord :: T.Text -> [T.Text] -> Bool
haystack `containsAnyWord` ns = any (haystack `containsWord`) ns

-- Does the haystack contain the needle, with word boundaries?
containsWord :: T.Text -> T.Text -> Bool
haystack `containsWord` needle = needle `elem` (T.words haystack)

-- Replace any of the rhymes with `word` in `phrase`
replaceAny :: T.Text -> [T.Text] -> T.Text -> T.Text
replaceAny _ [] word = error $ T.unpack ("Couldn't find a rhyme to replace in " <> word)
replaceAny phrase (rhyme:rs) word
    | phrase `containsWord` rhyme = replaceWord rhyme word phrase
    | otherwise = replaceAny phrase rs word

replaceWord :: T.Text -> T.Text -> T.Text -> T.Text
replaceWord from to = T.unwords . map swap . T.words
  where
    swap x
        | x == from = to
        | otherwise = x

solve :: T.Text -> [T.Text] -> [T.Text] -> [String]
solve originalWord rhymes phrases = map (show . makePun) matchingPhrases
    where
        makePun phrase = Pun (T.unpack phrase) (T.unpack (replaceAny phrase rhymes originalWord))
        matchingPhrases = filter (`containsAnyWord` rhymes) lowerPhrases
        lowerPhrases = map T.toLower phrases
