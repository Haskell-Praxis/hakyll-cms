-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
-- {-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE TypeFamilies              #-}

-- {-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE DataKinds     #-}
-- {-# LANGUAGE TypeOperators #-}



module Main where

import           Language.Javascript.JSaddle.Warp (run)
import           Reflex                           as R
import           Reflex.Dom.Old                   (MonadWidget)
import           Reflex.Dom.Main                  (mainWidgetWithCss)
import           Reflex.Dom.SimpleMDE
import           Reflex.Dom.Widget.Basic
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Language.Javascript.JSaddle.Types
import           Control.Monad.Fix
import           Data.Text                          (Text)
import           Data.ByteString                    (ByteString)
import           Data.Text                          as T
import           Data.Map.Lazy                      as Map
import           Control.Monad.IO.Class             (liftIO)

import           Hakyll.CMS.Types                  as Types
import           Hakyll.CMS.ServerCalls
import           Data.Maybe

-- for routing
import           Reflex.Dom.Contrib.Router
import           URI.ByteString                   hiding (uriFragment)
import           URI.ByteString                   as UBS
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Monoid                      ((<>))


-- for styling
import           Reflex.Dom.SemanticUI



getPostDemo :: IO ()
getPostDemo = do
  res <- getPostSummaries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> print response

-- intended usage:
-- - stream of events in to trigger calls
-- - stream of results and stream of errors out

-- overview:
-- - stream of [postsummary] in
-- - stream of refresh button and stream of goto-post out

-- post:


main :: IO ()
-- main = run 8081 $ mainWidgetWithCss simpleMdeCss page
main = run 8081 $ mainWidgetWithCss css app

css :: ByteString
css = encodeUtf8 semanticCSS <> simpleMdeCss

app :: forall t m. MonadWidget t m => m ()
app = do
    el "div" $ text "[ title bar ]"
    el "div" routedContent
    -- liftIO getPostDemo
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

routedContent :: MonadWidget t m => m ()
routedContent = do
  -- rec
    -- r <- partialPathRoute "" . switchPromptlyDyn =<< holdDyn never views
    -- gotoRoute <- dyn $ ffor r routingMappingOld -- don't need this. we're using fragment identifiers (i.e. the browser handles clicks on links for us)

  -- browser handles route changes for us, so we don't need to pass in an event to trigger that
  r <- route never
  let views = fmap routingMapping r
  dyn views
  return ()

routingMapping :: MonadWidget t m => URIRef Absolute -> m ()
routingMapping uri = case UBS.uriFragment uri of
      Nothing -> do
        -- renderOverview []
        startOverview
        return ()
      Just postId -> do
        -- let postId' = T.tail $ decodeUtf8 path
        let postId' = decodeUtf8 postId
        postEditView postId'
        return ()

startOverview :: MonadWidget t m =>  m ()
startOverview = do
  errOrSummaries <- liftIO getPostSummaries
  summaries <- either
    (\err -> do
        liftIO $ print ("Posts failed to load" :: Text)
        -- TODO error popup
        return []
      )
    (\summaries -> return summaries)
    errOrSummaries

  el "ul" $
    mapM_ postSummaryLine summaries

  return ()

postSummaryLine :: MonadWidget t m => PostSummary -> m ()
postSummaryLine postSummary = el "li" $ do
  let postViewFragment = "#" <> sumId postSummary
  (el, _) <- elAttr' "a" ("href" =: postViewFragment) $ text $ sumTitle postSummary
  -- return $ (postViewFragment <$ domEvent Click el)
  return ()

postEditView :: MonadWidget t m => Text -> m (Event t Text)
postEditView postId = do
  el "div" $ text $ "postId: " <> postId
  mdeChangeEvent <- simpleMDEWidget
  (ct :: Dynamic t Int) <- R.count mdeChangeEvent
  display ct
  text ("\n" :: Text)
  dynText =<< holdDyn "" mdeChangeEvent
  a <- button "toOverview"
  b <- button "Back"
  performEvent_ $  goBack <$ b
  return ("/" <$ a)


-- redefining the utility function contained in reflex-dom. as
-- we only use reflex-dom-core, we'll avoid including the former
-- just for this one function.
(=:) :: k -> a -> Map k a
(=:) = Map.singleton
