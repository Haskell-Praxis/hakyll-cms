{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.JSString ()
import           GHCJS.Types

foreign import javascript unsafe "window.alert($1)" js_alert :: JSString -> IO ()

main :: IO ()
main = js_alert "Hi"
