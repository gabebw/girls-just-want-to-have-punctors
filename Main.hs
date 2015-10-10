{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq

main = do
    r <- get "http://httpbin.org/get"
    print $ r ^. responseBody
