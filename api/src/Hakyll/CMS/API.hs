{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hakyll.CMS.API
    ( API
    , PostAPI
    )
    where

import           Data.Text
import           Data.Time
import           Hakyll.CMS.Types hiding (Post)
import qualified Hakyll.CMS.Types as Types
import           Servant.API

type API =
         ListPosts
    :<|> CreatePost
    :<|> Capture "post" Id :> PostAPI

type ListPosts =
    Get '[JSON] [PostSummary]

type CreatePost =
    ReqBody '[JSON] NewPost
        :> PostAccepted '[JSON] (Headers '[Header "Location" Text] Types.Post)

type PostAPI =
         Get '[JSON] Types.Post
    :<|> ReqBody '[JSON] Types.Post :> Put '[JSON] Types.Post
    :<|> DeleteNoContent '[JSON] NoContent
