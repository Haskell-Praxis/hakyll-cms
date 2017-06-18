-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecursiveDo               #-}
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
import           Reflex                           hiding (count)
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
import           Control.Monad.IO.Class             (liftIO)

-- for routing
import           Reflex.Dom.Contrib.Router
import           URI.ByteString                   hiding (uriFragment)
import           URI.ByteString                   as UBS
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Monoid                      ((<>))

-- for querying the server
import           Hakyll.CMS.API
import           Hakyll.CMS.Types                  as Types
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Network.HTTP.Client               (Manager, newManager, defaultManagerSettings)
import           Control.Monad.Trans.Except        (ExceptT, runExceptT)

-- for styling
import           Reflex.Dom.SemanticUI



api :: Proxy API
api = Proxy

listPosts :<|> createPost :<|> postApi = client api

getPost id =
  let getPost' :<|> _ :<|> _ = postApi id
  in getPost'

updatePost id =
  let _ :<|> updatePost' :<|> _ = postApi id
  in updatePost'

deletePost id =
  let _ :<|> _ :<|> deletePost' = postApi id
  in deletePost'

getPostDemo :: IO ()
getPostDemo = do
  manager <- newManager defaultManagerSettings
  -- res <- runExceptT $ getPost "testing-1_2017-01-08" manager (BaseUrl Http "localhost" 8080 "")
  res <- runExceptT $ listPosts manager (BaseUrl Http "localhost" 8080 "")
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> print response


main :: IO ()
-- main = run 8081 $ mainWidgetWithCss simpleMdeCss page
main = run 8081 $ mainWidgetWithCss css app

css :: ByteString
css = encodeUtf8 semanticCSS <> simpleMdeCss

app :: forall t m. MonadWidget t m => m ()
app = do
    el "div" $ text "[ title bar ]"
    el "div" routedContent
    liftIO getPostDemo
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
  -- rec r <- partialPathRoute "" . switchPromptlyDyn =<< holdDyn never views
  rec
    r <- route . switchPromptlyDyn =<< holdDyn never gotoRoute
    gotoRoute <- dyn $ ffor r routingMapping
  return ()

routingMapping :: MonadWidget t m => URIRef Absolute -> m (Event t Text)
routingMapping uri = case UBS.uriFragment uri of
      Nothing -> overview
      Just postId -> do
        -- let postId' = T.tail $ decodeUtf8 path
        let postId' = decodeUtf8 postId
        postEditView postId'

overview :: MonadWidget t m => m (Event t Text)
overview = do
  text " [ overview ] "
  a <- button "important announcement"
  b <- button "yet another blog entry"
  c <- button "lol, look at what i've found"

  return $ leftmost [
      "#a21342io35" <$ a,
      "#bas3dlf456" <$ b,
      "#07dn89s7gf" <$ c
    ]



renderPostSummaryLine :: MonadWidget t m => PostSummary -> m ()
renderPostSummaryLine postSummary = do
  let postViewFragment = "#" <> sumId postSummary
  elAttr "a" ("href" =: postViewFragment) $ text $ sumTitle postSummary
  return ()


-- viewA :: MonadWidget t m => m (Event t Text)
-- viewA = do
--   el "div" $ text "view A"
--   gotoB <- button "GoTo B"
--   return $ fmap (const "B") gotoB

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
