{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.CMS.Server
    ( server
    )
    where

import           Cases
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Map
import           Data.Maybe
import           Data.Monoid
import           Data.Sequences         hiding (pack)
import           Data.String            (String)
import           Data.Text
import           Data.Time
import           Data.Time.Format
import           Data.Tuple
import           Hakyll.CMS.API
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types       as Types
import           Prelude                (undefined)
import           Servant

api :: Proxy API
api = Proxy

server :: Application
server = serve api apiHandler

createDate :: String -> UTCTime
createDate = parseTimeOrError False defaultTimeLocale "%F"

postList :: [Types.Post]
postList =
    [ Types.Post
        { Types.title = "Testing 1"
        , author = "Sam Dent"
        , tags = ["tag1", "tag2"]
        , content = "Lorem Ipsum 1"
        , date = createDate "2017-01-08"
        }
    ]

posts :: Map Id Types.Post
posts =
    Data.Map.fromList $ fmap toPair postList
    where
        toPair post = (getId (Types.title post) (date post), post)

getId :: Title -> UTCTime -> Text
getId title date =
    let titlePart = spinalize title
        datePart = pack $ formatTime defaultTimeLocale "%F" date
    in
        titlePart <> "_" <> datePart

apiHandler :: Server API
apiHandler = listPosts :<|> createPost :<|> postServer

postFromCreation :: MonadIO m => NewPost -> m (Id, Types.Post)
postFromCreation post = do
    time <- liftIO getCurrentTime
    let id = getId (newTitle post) time
    return
        ( id
        , Types.Post
            { Types.title = newTitle post
            , author = newAuthor post
            , tags = newTags post
            , content = newContent post
            , date = time
            }
        )

createPost :: NewPost -> Handler (Headers '[Header "Location" Text] Types.Post)
createPost newPost = do
    (id, post) <- postFromCreation newPost
    when (member id posts) $
        throwError
            err409
                { errReasonPhrase = "Post already exists" }
    return $ addHeader ("/" <> id) post

listPosts :: Handler [PostSummary]
listPosts =
    return $ fmap (uncurry getSummary) $ toList posts

postServer :: Id -> Server PostAPI
postServer id = getPost id :<|> updatePost id :<|> deletePost id

getPost :: Id -> Handler Types.Post
getPost id =
    maybe (throwError err404) return (lookup id posts)

updatePost :: Id -> Types.Post -> Handler Types.Post
updatePost id post = do
    oldPost <- maybe (throwError err404) return (lookup id posts)
    when (date oldPost /= date post) $
        throwError
            err403
                { errReasonPhrase = "creation date is immutable" }
    return post

deletePost :: Id -> Handler NoContent
deletePost id = do
    post <- maybe (throwError err404) return (lookup id posts)
    return NoContent
