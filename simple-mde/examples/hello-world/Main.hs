{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom.Main                  (mainWidget)
import           Reflex.Dom.SimpleMDE
import           Reflex.Dom.Widget.Basic

main :: IO ()
main = run 8081 $ mainWidget $ do
  simpleMDEWidget
