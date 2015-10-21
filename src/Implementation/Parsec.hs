module Implementation.Parsec (solve) where

import Control.Monad (void)
import Control.Applicative (many)
import qualified Data.Text as T
import Text.Parsec (string, try, space, ParseError)
import qualified Text.Parsec as TP (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Common.Pun as Pun
import Implementation.Split (containsAnyWord)

-- Fail if it doesn't consume all input
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = TP.parse (p <* eof) ""

-- The parts of a phrase before/after a word, and the whole thing
data Phrase = Phrase String String String
    deriving Show

wordBoundary :: Parser String
wordBoundary = many space

-- `choice [p]` means "use the first parser that succeeds".
-- The `try` means "allow this parser to fail". We need it so a failing parser
-- doesn't consume input.
-- Combined, they mean "search through this list of parsers until you find one that succeeds".
-- At least one parser should succeed, but it's technically not guaranteed by
-- this implementation, which could let them all fail but still itself succeed.
phraseParser :: [String] -> Parser Phrase
phraseParser rhymes = choice $ map (try . parseAround) rhymes

parseAround :: String -> Parser Phrase
parseAround word = do
    before <- beforeWord word
    target <- string word
    after <- afterWord
    return $ Phrase before after (before ++ target ++ after)

beforeWord :: String -> Parser String
beforeWord word = do
    before <- manyTill anyToken (followedByWord word)
    boundary <- wordBoundary
    return (before ++ boundary)

afterWord :: Parser String
afterWord = many anyToken

followedByWord :: String -> Parser ()
followedByWord word = lookAhead $ try $ (void $ wordBoundary >> string word)

buildPun :: String -> Phrase -> Pun
buildPun word phrase@(Phrase _ _ originalPhrase) = Pun originalPhrase (punOf word phrase)

punOf :: String -> Phrase -> String
punOf word (Phrase before after _) = before ++ word ++ after

gracefullyParse :: String -> [String] -> String -> String
gracefullyParse originalWord rhymes p = either (failure p) success (parse rhymes p)
    where
        failure phrase err = phrase ++ "/" ++ show err
        success phrase = show $ buildPun originalWord phrase

parse :: [String] -> String -> Either ParseError Phrase
parse rhymes = parseWithEof (phraseParser rhymes)

solve :: T.Text -> [T.Text] -> [T.Text] -> [String]
solve originalWord rhymes phrases = map (gracefullyParse (T.unpack originalWord) rs) ps
    where
        ps = map T.unpack matchingPhrases
        matchingPhrases = filter (`containsAnyWord` rhymes) (map T.toLower phrases)
        rs = map T.unpack rhymes
