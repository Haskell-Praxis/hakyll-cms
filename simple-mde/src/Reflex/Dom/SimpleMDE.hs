{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}

module Reflex.Dom.SimpleMDE where

import           Control.Lens                       (makeLenses)
import           Control.Monad                      (when, void)
import           Data.Default
import           Data.FileEmbed
import           Data.Text                          (Text)
import           Data.ByteString                    (ByteString)
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types
import           Language.Javascript.JSaddle.Evaluate
import           Language.Javascript.JSaddle.Value
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Reflex.Dom.Widget.Basic
import           GHCJS.Marshal.Pure                 (pToJSVal, PToJSVal)
import           Reflex.Dom.Widget.Input
import           Reflex.PostBuild.Class


import           Reflex
import           Language.Javascript.JSaddle.Run    (syncPoint)
-- import           Reflex.Dynamic                     (holdDyn) -- TODO for debugging; remove me

data AutoSave = AutoSave
    { _autoSave_delay    :: Int
    , _autoSave_uniqueId :: String
    }

makeLenses ''AutoSave

data BlockStyles = BlockStyles
    { _blockStyles_bold   :: BoldBlockStyle
    , _blockStyles_code   :: CodeBlockStyle
    , _blockStyles_italic :: ItalicBlockStyle
    }

instance Default BlockStyles where
    def = BlockStyles
        { _blockStyles_bold = def
        , _blockStyles_code = def
        , _blockStyles_italic = def
        }

data BoldBlockStyle = TwoStars | TwoUnderscores

instance Default BoldBlockStyle where
    def = TwoStars

data CodeBlockStyle = Ticks | Wave

instance Default CodeBlockStyle where
    def = Ticks

data ItalicBlockStyle = SingleStar | SingleUnderscore

instance Default ItalicBlockStyle where
    def = SingleStar

data InsertTexts = InsertTexts
    { _insertTexts_horizontalRule :: Maybe (String, String)
    , _insertTexts_image          :: Maybe (String, String)
    , _insertTexts_link           :: Maybe (String, String)
    , _insertTexts_table          :: Maybe (String, String)
    }

makeLenses ''InsertTexts

instance Default InsertTexts where
    def = InsertTexts
        { _insertTexts_horizontalRule = Nothing
        , _insertTexts_image = Nothing
        , _insertTexts_link = Nothing
        , _insertTexts_table = Nothing
        }

data ParsingConfig = ParsingConfig
    { _parsingConfig_allowAtxHeaderWithoutSpace :: Bool
    , _parsingConfig_strikethrough              :: Bool
    , _parsingConfig_underscoresBreakWords      :: Bool
    }

makeLenses ''ParsingConfig

instance Default ParsingConfig where
    def = ParsingConfig
        { _parsingConfig_allowAtxHeaderWithoutSpace = False
        , _parsingConfig_strikethrough = True
        , _parsingConfig_underscoresBreakWords  = False
        }

data RenderingConfig = RenderingConfig
    { _renderingConfig_singleLineBreaks       :: Bool
    , _renderingConfig_codeSyntaxHighlighting :: Bool
    }

makeLenses ''RenderingConfig

instance Default RenderingConfig where
    def = RenderingConfig
        { _renderingConfig_singleLineBreaks = True
        , _renderingConfig_codeSyntaxHighlighting = False
        }

data SimpleMDEConfig = SimpleMDEConfig
    { _simpleMDEConfig_autoDownloadFontAwesome :: Maybe Bool
    , _simpleMDEConfig_autofocus               :: Bool
    , _simpleMDEConfig_autosave                :: Maybe AutoSave
    , _simpleMDEConfig_blockStyles             :: BlockStyles
    , _simpleMDEConfig_forceSync               :: Bool
    , _simpleMDEConfig_hideIcons               :: [String]
    , _simpleMDEConfig_indentWithTabs          :: Bool
    , _simpleMDEConfig_initialValue            :: String
    , _simpleMDEConfig_insertTexts             :: InsertTexts
    , _simpleMDEConfig_lineWrapping            :: Bool
    , _simpleMDEConfig_parsingConfig           :: ParsingConfig
    , _simpleMDEConfig_placeholder             :: Maybe String
    , _simpleMDEConfig_promptURLs              :: Bool
    , _simpleMDEConfig_renderingConfig         :: RenderingConfig
    , _simpleMDEConfig_shortcuts               :: Maybe [(String, String)]
    , _simpleMDEConfig_showIcons               :: [String]
    , _simpleMDEConfig_spellChecker            :: Bool
    , _simpleMDEConfig_status                  :: Maybe [String]
    , _simpleMDEConfig_styleSelectedText       :: Bool
    , _simpleMDEConfig_tabSize                 :: Int
    , _simpleMDEConfig_toolbar                 :: Maybe [String]
    , _simpleMDEConfig_toolbarTips             :: Bool
    }

makeLenses ''SimpleMDEConfig

instance Default SimpleMDEConfig where
    def = SimpleMDEConfig
        { _simpleMDEConfig_autoDownloadFontAwesome = Nothing
        , _simpleMDEConfig_autofocus = False
        , _simpleMDEConfig_autosave = Nothing
        , _simpleMDEConfig_blockStyles = def
        , _simpleMDEConfig_forceSync = False
        , _simpleMDEConfig_hideIcons = []
        , _simpleMDEConfig_indentWithTabs = True
        , _simpleMDEConfig_initialValue = ""
        , _simpleMDEConfig_insertTexts = def
        , _simpleMDEConfig_lineWrapping = True
        , _simpleMDEConfig_parsingConfig = def
        , _simpleMDEConfig_placeholder = Nothing
        , _simpleMDEConfig_promptURLs = False
        , _simpleMDEConfig_renderingConfig = def
        , _simpleMDEConfig_shortcuts = Nothing
        , _simpleMDEConfig_showIcons = []
        , _simpleMDEConfig_spellChecker = True
        , _simpleMDEConfig_status = Nothing
        , _simpleMDEConfig_styleSelectedText = True
        , _simpleMDEConfig_tabSize = 2
        , _simpleMDEConfig_toolbar = Nothing
        , _simpleMDEConfig_toolbarTips = True
        }

#ifdef ghcjs_HOST_OS
-- compiled with ghcjs; simpleMDE is bundled and executed by ghcjs
-- due to the js-sources entry in simple-mde.cabal. Thus
-- we don't need to do anything here.
importSimpleMdeJs :: JSM ()
importSimpleMdeJs = return ()

#else

-- Compiled with ghc, running in browser via jsaddle
-- As opposed to ghcjs, this setup doesn't automatically bundle
-- and run dependencies listed under js-sources in simple-mde.cabal
-- Thus we need to bundle the source via template-haskell and run
-- it via eval the first time this component is included.

-- | The contents of the upstream simpleMde js as a Text value.
simpleMdeCode :: Text
simpleMdeCode = $(embedStringFile "jslib/simplemde-markdown-editor/dist/simplemde.min.js")

importSimpleMdeJs :: JSM ()
importSimpleMdeJs = do
  simpleMDE <- jsg ("SimpleMDE" :: String)
  -- stop importing if it's already been imported
  notYetLoaded <- valIsUndefined simpleMDE
  when notYetLoaded $
    void $ eval simpleMdeCode

#endif

testFFI :: JSM ()
testFFI = do
    jsg ("console" :: String) # ("log" :: String) $ [("testing" :: String)]
    return ()

simpleMdeCss :: ByteString
simpleMdeCss = $(embedFile "jslib/simplemde-markdown-editor/dist/simplemde.min.css")

js_startSimpleMDE :: PToJSVal a => a -> JSM JSVal
js_startSimpleMDE el = do
  conf <- obj
  (conf <# ("element" :: Text)) (pToJSVal el)
  new (jsg ("SimpleMDE" :: Text)) (toJSVal conf)

  -- eval ("console.log('new SimpleMDE:', new SimpleMDE())" :: Text)



logEl :: PToJSVal a => a -> JSM ()
logEl el = do
  jsg ("console" :: String) # ("log" :: String) $ [ pToJSVal el]
  return ()

simpleMDEWidget :: (
    MonadHold t m, -- remove when there's no holdDyn anymore (atm it's just there for debugging)
    -- PerformEvent t m ~ JSM m,
    PerformEvent t m,
    MonadJSM (Performable m),
    MonadJSM m,
    DomBuilder t m,
    DomBuilderSpace m ~ GhcjsDomSpace,
    PostBuild t m
  ) => m ()
simpleMDEWidget = do
    liftJSM $ importSimpleMdeJs
    txtArea <- fmap fst $ elClass' "textarea" "simplemde-mount" blank
    let mdeEl = _element_raw txtArea
    liftJSM $ jsg ("console" :: String) # ("log" :: String) $ [ pToJSVal mdeEl]
    postBuildEvt <- getPostBuild

    dynText =<< holdDyn "" ("we're now post build!" <$ postBuildEvt)
    fooEvt <- return (("we're now post build!" :: String) <$ postBuildEvt)
    foo <- holdDyn (""::String) (("we're now post build!" :: String) <$ postBuildEvt)
    -- mdeEvt <- return (js_startSimpleMDE <$ postBuildEvt)

    -- bar <- performEvent (liftJSM js_startSimpleMDE <$ postBuildEvt) -- performEvent ~ fmap but allows side-effects. takes stream of performables, evaluates them, returns stream of results.
    -- bar <- performEvent (liftJSM (logEl mdeEl) <$ postBuildEvt) -- performEvent ~ fmap but allows side-effects. takes stream of performables, evaluates them, returns stream of results.
    -- bar <- performEvent (liftJSM (eval $ ("console.log('selected area: ', document.querySelector('textarea'))" :: Text)) <$ postBuildEvt) -- performEvent ~ fmap but allows side-effects. takes stream of performables, evaluates them, returns stream of results.
    bar <- performEvent (liftJSM (js_startSimpleMDE mdeEl) <$ postBuildEvt) -- performEvent ~ fmap but allows side-effects. takes stream of performables, evaluates them, returns stream of results.




    -- liftJSM syncPoint

    -- use PostBuild to make sure constructor is evaluated *after* textArea is created
    -- liftJSM $ eval $ ("new SimpleMDE()" :: Text)
    -- var s = new SimpleMDE({element: mdeEl})

    -- txtArea2 <- textArea def
    -- let mdeEl2 = _textArea_element txtArea2
    -- @pToJSVal for textarea: https://github.com/ghcjs/ghcjs-dom/blob/0e9e002e014a0058e8ed9e5d8159d3064510fb6c/ghcjs-dom-jsffi/src/GHCJS/DOM/JSFFI/Generated/HTMLTextAreaElement.hs#L43
    -- liftJSM $ jsg ("console" :: String) # ("log" :: String) $ [pToJSVal txtArea2]

    -- liftJSM testFFI
    return ()
