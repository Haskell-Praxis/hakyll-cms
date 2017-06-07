{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hakyll.CMS.API where

import           Data.Map
import           Data.Text
import           Data.Time
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types as Types
import           Servant.API

type API =
         ListPosts
    :<|> Capture "post" Name :> PostAPI

type ListPosts =
    Get '[JSON] [PostSummary]

type PostAPI =
         Get '[JSON] Types.Post
    :<|> ReqBody '[JSON] Types.Post :> Put '[] ()
    :<|> DeleteAccepted '[] ()
