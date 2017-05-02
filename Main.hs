{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           ClassyPrelude
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom

main :: IO ()
main = run 8081 $ mainWidget $ do
  el "h1" (text "A validation demo")
