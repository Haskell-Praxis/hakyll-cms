{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           ClassyPrelude
import           Hakyll.CMS.Server
import           Network.Wai.Handler.Warp


main :: IO ()
main = run 8080 server
