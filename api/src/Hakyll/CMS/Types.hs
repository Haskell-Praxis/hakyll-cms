{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hakyll.CMS.Types
    ( PostSummary(..)
    , Post(..)
    , NewPost(..)
    , Title
    , Author
    , Tag
    , getSummary
    , Id
    )
    where

import           Control.Category
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char        as Char
import           Data.Functor
import           Data.Sequences
import           Data.Text        (Text)
import           Data.Time
import           GHC.Generics

import           Text.Show        (Show)

type Tag = Text

type Author = Text

type Title = Text

type Id = Text

data PostSummary =
    PostSummary
        { sumId     :: Id
        , sumTitle  :: Title
        , sumAuthor :: Author
        , sumTags   :: [Tag]
        , sumDate   :: UTCTime
        , sumTeaser :: Text
        }
        deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fmap Char.toLower . drop 3, constructorTagModifier = fmap Char.toLower} ''PostSummary)

data Post =
    Post
        { title   :: Title
        , author  :: Author
        , tags    :: [Tag]
        , content :: Text
        , date    :: UTCTime
        }
        deriving (Generic, Show)

instance ToJSON Post

instance FromJSON Post

data NewPost =
    NewPost
        { newTitle   :: Title
        , newAuthor  :: Author
        , newTags    :: [Tag]
        , newContent :: Text
        }
        deriving (Show)


$(deriveJSON defaultOptions{fieldLabelModifier = fmap Char.toLower . drop 3, constructorTagModifier = fmap Char.toLower} ''NewPost)

getSummary :: Id -> Post -> PostSummary
getSummary id post =
    PostSummary
        { sumId = id
        , sumTitle = title post
        , sumAuthor = author post
        , sumTags = tags post
        , sumDate = date post
        , sumTeaser = take 200 (content post)
        }
