{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.CMS.Types
    ( NewPost
    , PostSummary
    , Post
    , Title
    , Id
    , Author
    , Tag
    )
    where

import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

type Tag = Text

type Author = Text

type Title = Text

type Id = Text

data NewPost =
    NewPost
      { newTitle   :: Title
      , newAuthor  :: Author
      , newTags    :: [Tag]
      , newCreated :: UTCTime
      , newContent :: Text
      }

instance ToJSON NewPost where
    toJSON newPost = object
        [ "title"    .= newTitle   newPost
        , "author"   .= newAuthor  newPost
        , "tags"     .= newTags    newPost
        , "created"  .= newCreated newPost
        , "content"  .= newContent newPost
        ]

instance FromJSON NewPost where
    parseJSON (Object o) =
        NewPost
            <$> o .: "title"
            <*> o .: "author"
            <*> o .: "tags"
            <*> o .: "created"
            <*> o .: "content"

data PostSummary =
    PostSummary
      { summaryId      :: Id
      , summaryTitle   :: Title
      , summaryAuthor  :: Author
      , summaryTags    :: [Tag]
      , summaryCreated :: UTCTime
      }
      deriving (Generic)

instance ToJSON PostSummary where
    toJSON summary = object
        [ "id" .= summaryId summary
        , "title" .= summaryTitle summary
        , "author" .= summaryAuthor summary
        , "tags" .= summaryTags summary
        , "created" .= summaryCreated summary
        ]

instance FromJSON PostSummary where
    parseJSON (Object o) =
        PostSummary
            <$> o .: "id"
            <*> o .: "title"
            <*> o .: "author"
            <*> o .: "tags"
            <*> o .: "created"

data Post =
    Post
      { id      :: Id
      , title   :: Title
      , author  :: Author
      , tags    :: [Tag]
      , created :: UTCTime
      , content :: Text
      }
      deriving (Generic)

instance ToJSON Post

instance FromJSON Post
