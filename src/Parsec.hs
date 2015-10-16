{-# LANGUAGE OverloadedStrings #-}
module Parsec where

import Control.Monad (void)
import Control.Applicative (many)
import Data.Monoid ((<>))
import Split (containsAnyWord)
import qualified Data.Text as T
import Text.Parsec (string, try, space, ParseError)
import qualified Text.Parsec as TP (parse)
import Text.Parsec.T.Text (Parser)
import Text.Parsec.Combinator

-- Fail if it doesn't consume all input
parseWithEof :: Parser a -> T.Text -> Either ParseError a
parseWithEof p = TP.parse (p <* eof) ""

-- The parts of a phrase before/after a word, and the whole thing
data Phrase = Phrase T.Text T.Text T.Text
    deriving Show

-- The original phrase and the pun phrase
data Pun = Pun T.Text T.Text

instance Show Pun where
    show (Pun original pun) = pun ++ " (pun of \"" ++ original ++ "\")"

wordBoundary :: Parser T.Text
wordBoundary = many space

-- `choice [p]` means "use the first parser that succeeds".
-- The `try` means "allow this parser to fail". We need it so a failing parser
-- doesn't consume input.
-- Combined, they mean "search through this list of parsers until you find one that succeeds".
-- At least one parser should succeed, but it's technically not guaranteed by
-- this implementation, which could let them all fail but still itself succeed.
phraseParser :: [T.Text] -> Parser Phrase
phraseParser rhymes = choice $ map (try . parseAround) rhymes

parseAround :: T.Text -> Parser Phrase
parseAround word = do
    before <- beforeWord word
    target <- string word
    after <- afterWord
    return $ Phrase before after (before ++ target ++ after)

beforeWord :: T.Text -> Parser T.Text
beforeWord word = do
    before <- manyTill anyToken (followedByWord word)
    boundary <- wordBoundary
    return (before ++ boundary)

afterWord :: Parser T.Text
afterWord = many anyToken

followedByWord :: T.Text -> Parser ()
followedByWord word = lookAhead $ try $ (void $ wordBoundary >> string word)

buildPun :: T.Text -> Phrase -> Pun
buildPun word phrase@(Phrase _ _ originalPhrase) = Pun originalPhrase (punOf word phrase)

punOf :: T.Text -> Phrase -> T.Text
punOf word (Phrase before after _) = before ++ word ++ after

gracefullyParse :: T.Text -> [T.Text] -> T.Text -> T.Text
gracefullyParse originalWord rhymes p = either (failure p) success (parse rhymes p)
    where
        failure phrase err = phrase ++ "/" ++ show err
        success phrase = show $ buildPun originalWord phrase

parse :: [T.Text] -> T.Text -> Either ParseError Phrase
parse rhymes = parseWithEof (phraseParser rhymes)

solve :: T.Text -> [T.Text] -> [T.Text] -> [T.Text]
solve originalWord rhymes phrases = map (gracefullyParse originalWord) rs) ps
    where
        ps = filter (`containsAnyWord` rhymes) $ map T.toLower phrases
        rs = rhymes
