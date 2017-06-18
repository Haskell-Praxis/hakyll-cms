module Hakyll.CMS.ServerCalls where

-- for querying the server
import           Hakyll.CMS.API
import           Hakyll.CMS.Types                  as Types
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Network.HTTP.Client               (Manager, newManager, defaultManagerSettings)
import           Control.Monad.Trans.Except        (ExceptT, runExceptT)

api :: Proxy API
api = Proxy

getPostSummariesGeneric :<|> createPostGeneric :<|> postApi = client api

getPostGeneric id =
  let getPostGeneric' :<|> _ :<|> _ = postApi id
  in getPostGeneric'

updatePostGeneric id =
  let _ :<|> updatePostGeneric' :<|> _ = postApi id
  in updatePostGeneric'

deletePostGeneric id =
  let _ :<|> _ :<|> deletePostGeneric' = postApi id
  in deletePostGeneric'

serverUrl :: BaseUrl
serverUrl = BaseUrl Http "localhost" 8080 ""

callServer :: (Manager -> BaseUrl -> ExceptT e IO a) -> IO (Either e a)
callServer apiCall = do
  manager <- newManager defaultManagerSettings
  runExceptT $ apiCall manager serverUrl

getPostSummaries :: IO (Either ServantError [PostSummary])
getPostSummaries = callServer getPostSummariesGeneric

createPost :: NewPost -> IO (Either ServantError ())
createPost = callServer . createPostGeneric

getPost :: Id -> IO (Either ServantError Types.Post)
getPost = callServer . getPostGeneric

updatePost :: Id -> Types.Post -> IO (Either ServantError ())
updatePost id post = callServer $ updatePostGeneric id post

deletePost :: Id -> IO (Either ServantError ())
deletePost = callServer . deletePostGeneric
