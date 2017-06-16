{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}


-- from routing example TODO deletme
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}




module Main where

import           Language.Javascript.JSaddle.Warp (run)
import           Reflex
import           Reflex                           as R
import           Reflex.Dom.Old                   (MonadWidget)
import           Reflex.Dom.Main                  (mainWidgetWithCss)
import           Reflex.Dom.SimpleMDE
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate

import           Reflex.Dom.Contrib.MonadRouted -- we probably only need one of these
import           Reflex.Dom.Contrib.Router -- we probably only need one of these
import           URI.ByteString
import            Data.Text.Encoding              (decodeUtf8)



import           Language.Javascript.JSaddle.Types
import           Control.Monad.Fix
import           Data.Text                          (Text)
import           Data.Text                          as T

main :: IO ()
-- main = run 8081 $ mainWidgetWithCss simpleMdeCss page
main = run 8081 $ mainWidgetWithCss simpleMdeCss app

app :: forall t m. MonadWidget t m => m ()
app = do
    el "div" $ text "[ title bar ]"
    el "div" $ routerExample
    return ()


  --   >>> parseURI strictURIParserOptions "http://www.example.org/foo?bar=baz#quux"
  -- Right (URI {
  --   uriScheme = Scheme {schemeBS = "http"},
  --   uriAuthority = Just (Authority {
  --     authorityUserInfo = Nothing,
  --     authorityHost = Host {hostBS = "www.example.org"},
  --     authorityPort = Nothing
  --   }),
  --   uriPath = "/foo",
  --   uriQuery = Query {
  --     queryPairs = [("bar","baz")]
  --   },
  --   uriFragment = Just "quux"
  -- })
  -- pathL?
  -- queryL? allows using query-pairs

routerExample :: MonadWidget t m => m ()
routerExample = do
  -- rec r <- partialPathRoute "" . switchPromptlyDyn =<< holdDyn never views
  rec
    r <- route . switchPromptlyDyn =<< holdDyn never views
    views <- dyn $ ffor r $ \uri -> case (uriPath uri) of
      "/" -> do
        text " [ overview ] "
        a <- button "important announcement"
        b <- button "yet another blog entry"
        c <- button "lol, look at what i've found"

        return $ leftmost [
            "a21342io35" <$ a,
            "bas3dlf456" <$ b,
            "07dn89s7gf" <$ c
          ]
      path -> do
        let postId = T.tail $ decodeUtf8 path
        text postId
        a <- button "toOverview"
        b <- button "Back"
        performEvent_ $  goBack <$ b
        return ("/" <$ a)
  return ()

-- viewA :: MonadWidget t m => m (Event t Text)
-- viewA = do
--   el "div" $ text "view A"
--   gotoB <- button "GoTo B"
--   return $ fmap (const "B") gotoB
























-- page :: (
--     MonadJSM m,
--     DomBuilder t m,
--     DomBuilderSpace m ~ GhcjsDomSpace,
--     TriggerEvent t m,
--     MonadFix m,
--     MonadHold t m,
--     PostBuild t m
--   ) => m ()
page :: forall t m. MonadWidget t m => m ()
page = do
  el "div" $ text "[title bar?]"
  el "div" $ routedContent
  return ()

basePath :: Text
basePath = ""

routedContent :: forall t m. MonadWidget t m => m ()
-- routedContent :: (
--     MonadJSM m,
--     DomBuilder t m,
--     DomBuilderSpace m ~ GhcjsDomSpace,
--     TriggerEvent t m,
--     MonadFix m,
--     MonadHold t m,
--     PostBuild t m
--
--     -- MonadWidgetContraints t m
--
--   ) => m ()
routedContent = do
    rec
        let routingUpdates = never
        -- r <- partialPathRoute basePath routingUpdates
    --     dynText $ fmap T.unlines r

        -- r <- partialPathRoute basePath . switchPromptlyDyn =<< holdDyn never views
        -- views <- dyn $ ffor r $ pathToView
    return ()


pathToView path = case path of
  [""] -> text "[ overview over all posts ]"
  ["new"] -> text "[ new post ]"
  [postId] -> text "[ edit post ]"
  _ -> text "[ 404 ]"

postEditView :: (
    MonadJSM m,
    DomBuilder t m,
    DomBuilderSpace m ~ GhcjsDomSpace,
    TriggerEvent t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m
  ) => m ()
postEditView = do
  mdeChangeEvent <- simpleMDEWidget
  (ct :: Dynamic t Int) <- R.count mdeChangeEvent
  display ct
  text ("\n" :: Text)
  dynText =<< holdDyn "" mdeChangeEvent
  -- Reflex.Dom.Builder.Class.DomBuilderSpace m’
  --                with ‘Reflex.Dom.Builder.Immediate.GhcjsDomSpace’

-- either use `Router` and code at https://github.com/reflex-frp/reflex-dom-contrib/issues/28
-- or use `MonadRouted`

-- Router uses `/` to seperate client-side routing instead of fragment identifiers for some reason
-- we could use `route'` to strip fragment identifiers before handing the route over to `Router` (and add it when reading from it)

-- i guess monadrouted is the more complex/advanced of the two and we'd need it if we'd do
-- more complex path manipulation (e.g. relativ path changes, path's processed by nested widgets, etc)
-- it works on path parts.

-- nvm, both of these use monadwidget (which is deprecated). We might need to write our own routing.
-- it only appears as constraint though
-- solution: we could simply see which of the following constraints that make up MonadWidgetConstraint
--   are necessary to compile the router https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom-core/src/Reflex/Dom/Old.hs#L107
