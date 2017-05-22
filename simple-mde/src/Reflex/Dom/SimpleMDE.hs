{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.SimpleMDE where

import           Control.Lens
import           Data.Default
import  Reflex.Dom

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

simpleMDEWidget :: MonadWidget t m => m ()
simpleMDEWidget = undefined
