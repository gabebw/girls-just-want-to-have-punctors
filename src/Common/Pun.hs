module Common.Pun where

-- The original phrase and the pun phrase
data Pun = Pun String String

instance Show Pun where
    show (Pun original pun) = pun ++ " (pun of \"" ++ original ++ "\")"
