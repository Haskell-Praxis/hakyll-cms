module Hakyll.CMS.Server
    ( server
    )
    where

import           Cases
import           Data.Text
import           Data.Time.Format
import           Hakyll.CMS.API
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types as Types
import           Servant

server :: Server API
server = listPosts :<|> createPost :<|> postServer

createPost :: NewPost -> Handler ()
createPost newPost = do
    undefined
    where
        getTitle = snakify $ newTitle newPost

listPosts :: Handler [PostSummary]
listPosts = undefined

postServer :: Id -> Server PostAPI
postServer id = getPost id :<|> updatePost id :<|> deletePost id

getPost :: Id -> Handler Types.Post
getPost = undefined

updatePost :: Id -> Types.Post -> Handler ()
updatePost = undefined

deletePost :: Id -> Handler ()
deletePost = undefined
