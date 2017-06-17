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
    )
    where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char      as Char
import           Data.Functor
import           Data.Sequences
import           Data.Text      (Text)
import           Data.Time
import           GHC.Generics

type Tag = Text

type Author = Text

type Title = Text

data PostSummary =
    PostSummary
        { sumTitle  :: Title
        , sumAuthor :: Author
        , sumTags   :: [Tag]
        , sumDate   :: UTCTime
        , sumTeaser :: Text
        }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = fmap Char.toLower} ''PostSummary)

data Post =
    Post
        { title   :: Title
        , author  :: Author
        , tags    :: [Tag]
        , content :: Text
        , date    :: UTCTime
        }
        deriving (Generic)

instance ToJSON Post

instance FromJSON Post

data NewPost =
    NewPost
        { newTitle   :: Title
        , newAuthor  :: Author
        , newTags    :: [Tag]
        , newContent :: Text
        }


$(deriveJSON defaultOptions{fieldLabelModifier = drop 3, constructorTagModifier = fmap Char.toLower} ''NewPost)

getSummary :: Post -> PostSummary
getSummary post =
    PostSummary
        { sumTitle = title post
        , sumAuthor = author post
        , sumTags = tags post
        , sumDate = date post
        , sumTeaser = take 200 (content post)
        }
