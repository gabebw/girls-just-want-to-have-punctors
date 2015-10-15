{-# LANGUAGE OverloadedStrings #-}

{-

The high-level idea behind this implementation:

* Given an original word, like "cart"
* And a list of rhymes, like "heart" and "start"
* And some phrases, like "put your heart in it" and "kick start"
* We want to create "put your cart into it" and "kick cart"
* So, we split on each rhyme, which gives us the pieces before and after each
  rhyme, then put the pieces back together with cart.
* Example: ["put your ", "into it"] -> "put your cart into it"

We lowercase everything to make comparing easier, and don't un-lowercase
because it's tricky (if it was "Heart", then we'd need to ensure the pun is
"Cart", not "cart") and I don't want to.

Splitting on any one of a list of words rather than a specific one is interesting,
sure, but why write all this?

Glad you asked.

The trickiness comes with word boundaries: we don't want to split "hearth" even
though it contains "heart". So (heavy sigh) we must create our own word boundaries,
like the (Perl compatible) regular expression /\bheart\b/.

So we have a list of wordBoundaryCharacters, and we use List's applicative
instance (<*>) to create all possible word boundaries.  Given a [T.Text] of word
boundary chars, we create all possible combinations where they are on the left
of the word, and all possible combinations where they are on the right of the
word, and combine _those_ combinations.

So it creates `heart"` (word boundary char only on the right), then `"heart"`
(word boundary char on both sides), for all word boundary chars.
-}

module Split where

import Data.Monoid ((<>))
import qualified Data.Text as T

-- Does the text contain any of the words, with word boundaries?
containsAnyWord :: T.Text -> [T.Text] -> Bool
_ `containsAnyWord` [] = False
haystack `containsAnyWord` (s:ss)
    | haystack `containsWord` s = True
    | otherwise = haystack `containsAnyWord` ss

-- Does the haystack contain a given needle string, with word boundaries?
-- So this returns False:
-- > "Martini" `containsWord` "art"
-- while this returns True:
-- > "Art, is, cool" `containsWord` "art"
containsWord :: T.Text -> T.Text -> Bool
haystack `containsWord` needle = any (`T.isPrefixOf` haystack) (rightBounded needle) ||
    any (`T.isSuffixOf` haystack) (leftBounded needle) ||
    any (`T.isInfixOf` haystack) (withWordBoundaries needle)

-- Generate all possible combinations of this word with a word boundary
-- character on its left and right.
-- So it generates ` word `, ` word,`, `?word!`, etc.
-- In regex terms, it's manually generating /\bword\b/
withWordBoundaries :: T.Text -> [T.Text]
withWordBoundaries word = left <*> (rightBounded word)
    where
        -- `word` isn't in `left` because then it would be
        -- doubled in the final result.
        left = [(<>)] <*> wordBoundaryCharacters

-- All possible versions of a word that _end_ with a word boundary char.
rightBounded :: T.Text -> [T.Text]
rightBounded word = [(word <>)] <*> wordBoundaryCharacters

-- All possible versions of a word that _start_ with a word boundary char.
leftBounded :: T.Text -> [T.Text]
leftBounded word = [(<> word)] <*> wordBoundaryCharacters

wordBoundaryCharacters :: [T.Text]
wordBoundaryCharacters = map T.singleton " \"().,!'?-"

-- Split on any one of a given needle, and error if we can't (but we should be
-- able to, because we filter to only matching text before this)
splitOnAny :: T.Text -> [T.Text] -> [T.Text]
text `splitOnAny` [] = error $ T.unpack ("Couldn't split for " <> text)
text `splitOnAny` (n:ns)
    | length split == 1 = text `splitOnAny` ns
    | otherwise = split
    where
        split = T.splitOn n text

solve :: T.Text -> [T.Text] -> [T.Text] -> [String]
solve originalWord rhymes phrases = map (T.unpack . makePun) matchingPhrases
    where
        makePun phrase = T.intercalate originalWord $ phrase `splitOnAny` rhymes
        matchingPhrases = filter (`containsAnyWord` rhymes) lowerPhrases
        lowerPhrases = map T.toLower phrases
