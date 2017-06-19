-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
-- {-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
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
import           Reflex.Dom.Widget.Input
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Language.Javascript.JSaddle.Types
import           Control.Monad.Fix
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.ByteString                    (ByteString)
import           Data.Map.Lazy                      (Map)
import qualified Data.Map.Lazy                      as Map
import           Control.Monad.IO.Class             (liftIO)

import           Hakyll.CMS.Types                  as Types
import           Hakyll.CMS.ServerCalls
import           Data.Maybe
import           Data.FileEmbed

import           Reflex.Dom.SemanticUI
import           Reflex.Dom.SemanticUI.Input
import           Data.Default
import           Control.Lens.Operators             ((.~), (&))

-- for routing
import           Reflex.Dom.Contrib.Router
import           URI.ByteString                   hiding (uriFragment)
import           URI.ByteString                   as UBS
import           Data.Text.Encoding               (decodeUtf8, encodeUtf8)
import           Data.Monoid                      ((<>))




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
css = encodeUtf8 semanticCSS <>
      simpleMdeCss <>
      clientCss

clientCss :: ByteString
clientCss = $(embedFile "static/client.css")

appTitle :: Text
appTitle = "Hakyll CMS"

app :: forall t m. MonadWidget t m => m ()
app = do
    header
    divClass "ui main text container" routedContent
    -- liftIO getPostDemo
    return ()


header :: forall t m. MonadWidget t m => m ()
header = divClass "ui fixed inverted menu" $
  divClass "ui container" $
    elAttr "a" (
      ("class" =: "header item") <>
      ("href" =: "#")
    ) $ do
      -- TODO needs bundling (or static serving)
      elAttr "img" (
          ("class" =: "logo") <>
          ("src" =: "static/logo.export.svg")
        ) blank
      text appTitle

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
      Nothing -> loadOverview
      Just "" -> loadOverview
      Just postId -> do
        -- let postId' = T.tail $ decodeUtf8 path
        let postId' = decodeUtf8 postId
        loadPostEditView postId'
        return ()

loadOverview :: MonadWidget t m => m ()
loadOverview = do
  errOrSummaries <- liftIO $ getPostSummaries
  case errOrSummaries of
    -- TODO popup with proper error message
    Left _ -> text $ "Failed to load post summaries"
    Right summaries -> overview summaries

overview :: MonadWidget t m => [PostSummary] -> m ()
overview summaries = do
  elClass "h1" "ui header" $ text "All Posts"
  elClass "ul" "ui items" $
    mapM_ postSummaryLine summaries

  return ()

postSummaryLine :: MonadWidget t m => PostSummary -> m ()
postSummaryLine postSummary =
  elClass "li" "item" $ do
    let imageUri = "https://foxrudor.de/?q=" <> sumId postSummary -- id appended so every post has a different image (i.e. to avoid caching)
    divClass "image" $ elAttr "img" ("src" =: imageUri) $ blank
    divClass "content" $ do

      let postViewFragment = "#" <> sumId postSummary
      elAttr "a" (
          ("class" =: "header") <>
          ("href" =: postViewFragment)
        ) $
        text $ sumTitle postSummary
      -- return $ (postViewFragment <$ domEvent Click el) -- use (el,_) <- elAttr` above
      divClass "description" $
        el "p" $
          text $ sumTeaser postSummary
      return ()

loadPostEditView :: MonadWidget t m => Id -> m ()
loadPostEditView postId = do
  errOrPost <- liftIO $ getPost postId
  case errOrPost of
    -- TODO popup with proper error message
    Left _ -> text $ "Failed to load post with id " <> postId
    Right post -> postEditView postId post

postEditView :: MonadWidget t m => Id -> Post -> m ()
postEditView postId post = do
  elClass "h1" "ui header" $ text $ "Editing \"" <> postId <> "\""

  elClass "form" "ui form" $ do

    titleTxtInput <- uiLabelledTextInput "Title" (constDyn def) (def & textInputConfig_initialValue .~ (title post))
    let titleD = value titleTxtInput

    authorTxtInput <- uiLabelledTextInput "Author(s)" (constDyn def) (def & textInputConfig_initialValue .~ (author post))
    let authorD = value authorTxtInput

    let tagsInitVal = T.intercalate ", " $ tags post
    tagsTxtInput <- uiLabelledTextInput "Tags" (constDyn def) (def & textInputConfig_initialValue .~ tagsInitVal)
    let splitTags = \ts -> map T.strip $ T.split (==',') ts
    let tagsD = fmap splitTags $ value tagsTxtInput

    contentD <- divClass "field" $ do
      el "label" $ text "Content"
      simpleMDEWidget $ content post

    let dateD = constDyn $ date post
    let postD = Post <$> titleD <*> authorD <*> tagsD <*> contentD <*> dateD

    dynText $ fmap (T.pack . show) postD

    submitBtnE <- uiButton def $ text "Save"
    let submitBtnPostE = tagPromptlyDyn postD submitBtnE
    let submitE = ffor submitBtnPostE $ (\post' -> liftIO $ updatePost postId post')
    performEvent submitE

    return ()

  return ()

uiLabelledTextInput :: MonadWidget t m
  => Text
  -> Dynamic t UiInput
  -> TextInputConfig t
  -> m (TextInput t)
uiLabelledTextInput label iDyn c = divClass "field" $ do
    el "label" $ text label
    uiTextInput iDyn c

-- redefining the utility function contained in reflex-dom. as
-- we only use reflex-dom-core, we'll avoid including the former
-- just for this one function.
(=:) :: k -> a -> Map k a
(=:) = Map.singleton



dummyNewPost :: NewPost
dummyNewPost = NewPost
        { newTitle = "New Dummy Post"
        , newAuthor = "Sam Dent"
        , newTags = ["tag1", "tag2"]
        , newContent = "Some new dummy post, vestibulum sed placerat odio. Phasellus pulvinar ex in lorem auctor congue. Maecenas egestas auctor nisl, eget efficitur neque condimentum et. Ut auctor auctor molestie. Vestibulum porta urna sapien, eget iaculis velit bibendum sit amet. Pellentesque venenatis sapien in ligula egestas, eu aliquet orci laoreet."
        }
