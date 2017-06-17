{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hakyll.CMS.API
    ( API
    , PostAPI
    , Id
    )
    where

import           Data.Map
import           Data.Text
import           Data.Time
import           Hakyll.CMS.Types hiding (Post)
import qualified Hakyll.CMS.Types as Types
import           Servant.API

type Id = Text

type API =
         ListPosts
    :<|> CreatePost
    :<|> Capture "post" Id :> PostAPI

type ListPosts =
    Get '[JSON] [PostSummary]

type CreatePost =
    ReqBody '[JSON] NewPost :> Post '[JSON] ()

type PostAPI =
         Get '[JSON] Types.Post
    :<|> ReqBody '[JSON] Types.Post :> Put '[JSON] ()
    :<|> DeleteAccepted '[JSON] ()
