{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

module Main where

import           ClassyPrelude
import           Language.Javascript.JSaddle.Warp (run)

import           Data.FileEmbed

import           Reflex
import           Reflex.Dom

main :: IO ()
main = run 8081 $ mainWidgetWithCss importedCss
  $ do
    el "h1" (text "A validation demo")
    -- t <- textInput def
    -- dynText $ _textInput_value t

importedCss :: ByteString
importedCss = $(embedFile "./style.css")
