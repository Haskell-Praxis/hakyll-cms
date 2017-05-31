{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hakyll.CMS.API where

import           Data.Text
import           Data.Time
import           Servant.API

type API = GetPost
      :<|> GetPosts
      :<|> CreatePost
      :<|> DeletePost

-- URL: /new
type CreatePost = "new"
                    :> ReqBody '[JSON] BlogPost
                    :> PostCreated '[JSON] BlogPost

-- URL: /
type GetPosts = Get '[JSON] [BlogPost]

-- URL: /$post
type GetPost = Capture "post" Text
                :> Get '[JSON] BlogPost

type DeletePost = Capture "post" Text
                    :> DeleteAccepted




type Author = Text
type Tag = Text

data BlogPost = BlogPost
  { author  :: Maybe Author
  , created :: UTCTime
  , title   :: Text
  , tags    :: [Tag]
  , body    :: Text
  }
