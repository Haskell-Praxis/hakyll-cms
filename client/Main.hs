{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
-- {-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex.Dom.Main                  (mainWidgetWithCss)
import           Reflex.Dom.SimpleMDE
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate

import           Reflex.Dom.Contrib.MonadRouted -- we probably only need one of these
import           Reflex.Dom.Contrib.Router -- we probably only need one of these

import           Language.Javascript.JSaddle.Types
import           Control.Monad.Fix
import           Data.Text                          (Text)

main :: IO ()
main = run 8081 $ mainWidgetWithCss simpleMdeCss mainWidget

mainWidget :: (
    MonadJSM m,
    DomBuilder t m,
    DomBuilderSpace m ~ GhcjsDomSpace,
    TriggerEvent t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) => m ()
mainWidget = do
  mdeChangeEvent <- simpleMDEWidget
  (ct :: Dynamic t Int) <- count mdeChangeEvent
  display ct
  text ("\n" :: Text)
  dynText =<< holdDyn "" mdeChangeEvent
  -- Reflex.Dom.Builder.Class.DomBuilderSpace m’
  --                with ‘Reflex.Dom.Builder.Immediate.GhcjsDomSpace’
