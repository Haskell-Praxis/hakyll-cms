{-# LANGUAGE TypeFamilies #-}

module Hakyll.CMS.Server

where

import           Control.Monad.Catch        hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Map
import           Data.Text
import           Hakyll.CMS.API
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types           as Types
import           Path
import           Path.IO
import           Servant

server :: Server API
server = listPosts :<|> postServer

listPosts :: Handler [PostSummary]
listPosts = undefined

postServer :: Name -> Server PostAPI
postServer id = getPost id :<|> updatePost id :<|> deletePost id

getPost :: Name -> Handler Types.Post
getPost = undefined

updatePost :: Name -> Types.Post -> Handler ()
updatePost = undefined

deletePost :: Name -> Handler ()
deletePost = undefined


loadPost
  :: (MonadIO m, MonadReader m, EnvType m ~ (Path b Dir), MonadThrow m)
  => Name
  -> m Types.Post
loadPost id = do
  dir      <- ask
  fileName <- parseRelFile (unpack id) >>= setFileExtension ".markdown"
  file     <- findFile [dir] fileName
  return undefined
