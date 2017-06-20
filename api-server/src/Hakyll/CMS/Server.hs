{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.CMS.Server
    ( server
    )
    where

import           Cases
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Bool
import           Data.Eq
import           Data.Function
import           Data.Map
import           Data.Maybe
import           Data.Monoid
import           Data.Sequences              hiding (pack)
import           Data.String                 (String)
import           Data.Text
import           Data.Time
import           Data.Time.Format
import           Data.Tuple
import           Hakyll.CMS.API
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types            as Types
import           Prelude                     (undefined)
import           Servant

api :: Proxy API
api = Proxy

server :: TVar PostList -> Application
server tvar = serve api $ apiHandler tvar

createDate :: String -> UTCTime
createDate = parseTimeOrError False defaultTimeLocale "%F"

type PostList = Map Id Types.Post

getId :: Title -> UTCTime -> Text
getId title date =
    let titlePart = spinalize title
        datePart = pack $ formatTime defaultTimeLocale "%F" date
    in
        titlePart <> "_" <> datePart

apiHandler :: TVar PostList -> Server API
apiHandler tvar = listPosts tvar :<|> createPost tvar :<|> postServer tvar

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

createPost :: TVar PostList -> NewPost -> Handler (Headers '[Header "Location" Text] Types.Post)
createPost tvar newPost = do
    (id, post) <- postFromCreation newPost
    didFail <- liftIO $ atomically $ do
        currentState <- readTVar tvar
        if member id currentState then
            return True
        else do
            writeTVar tvar (insert id post currentState)
            return False

    when didFail $
        throwError
            err409
                { errReasonPhrase = "Post already exists" }
    return $ addHeader ("/" <> id) post

listPosts :: TVar PostList -> Handler [PostSummary]
listPosts tvar = do
    posts <- liftIO $ atomically $ readTVar tvar
    return $ fmap (uncurry getSummary) $ toList posts

postServer :: TVar PostList -> Id -> Server PostAPI
postServer tvar id = getPost tvar id:<|> updatePost tvar id :<|> deletePost tvar id

getPost :: TVar PostList ->  Id -> Handler Types.Post
getPost tvar id = do
    posts <- liftIO $ atomically $ readTVar tvar
    maybe (throwError err404) return (lookup id posts)

updatePost :: TVar PostList -> Id -> Types.Post -> Handler Types.Post
updatePost tvar id post = do
    err <- liftIO $ atomically $ do
        currentState <- readTVar tvar
        case lookup id currentState of
            Just oldPost ->
                if date oldPost /= date post
                then
                    return $ Just
                        err403
                            { errReasonPhrase = "creation date is immutable" }
                else do
                    writeTVar tvar $ insert id post currentState
                    return Nothing
            Nothing -> return $ Just err404
    maybe (return post) throwError err

deletePost :: TVar PostList -> Id -> Handler NoContent
deletePost tvar id = do
    found <- liftIO $ atomically $ do
        currentState <- readTVar tvar
        if member id currentState
        then do
            writeTVar tvar $ Data.Map.delete id currentState
            return True
        else
            return False
    unless found $ throwError err404
    return NoContent
