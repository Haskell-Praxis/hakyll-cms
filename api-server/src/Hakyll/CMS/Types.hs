{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.CMS.Types where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

type Tag = Text

type Author = Text

type Name = Text

data PostSummary =
    PostSummary
      { summaryName    :: Name
      , summaryAuthor  :: Author
      , summaryTags    :: [Tag]
      , summaryCreated :: UTCTime
      }
      deriving (Generic)

instance ToJSON PostSummary where
    toJSON summary = object
        [ "name" .= summaryName summary
        , "author" .= summaryAuthor summary
        , "tags" .= summaryTags summary
        , "created" .= summaryCreated summary
        ]

instance FromJSON PostSummary where
    parseJSON (Object o) =
        PostSummary
            <$> o .: "name"
            <*> o .: "author"
            <*> o .: "tags"
            <*> o .: "created"

data Post =
    Post
      { author  :: Author
      , tags    :: [Tag]
      , created :: UTCTime
      , content :: Text
      }
      deriving (Generic)

instance ToJSON Post

instance FromJSON Post
