{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.CMS.Server
    ( server
    )
    where

import           Cases
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Map
import           Data.Monoid
import           Data.Sequences         hiding (pack)
import           Data.Text
import           Data.Time
import           Data.Time.Format
import           Data.Tuple
import           Hakyll.CMS.API
import           Hakyll.CMS.Types
import qualified Hakyll.CMS.Types       as Types
import           Prelude                (maybe, undefined, ($))
import           Servant

api :: Proxy API
api = Proxy

server :: Application
server = serve api apiHandler

posts :: MonadIO m => m (Map Id Types.Post)
posts = do
    time <- liftIO getCurrentTime
    return
        (Data.Map.fromList
            [ ( "testing-1_2017-01-08"
              , Types.Post
                  { Types.title = "Testing 1"
                  , author = "Sam Dent"
                  , tags = ["tag1", "tag2"]
                  , content = "Lorem Ipsum 1"
                  , date = time
                  }
              )
            ]
        )

getId :: Title -> UTCTime -> Text
getId title date =
    let titlePart = spinalize title
        datePart = pack $ formatTime defaultTimeLocale "%F" date
    in
        titlePart <> "_" <> datePart

apiHandler :: Server API
apiHandler = listPosts :<|> createPost :<|> postServer

createPost :: NewPost -> Handler ()
createPost newPost = throwError err500

listPosts :: Handler [PostSummary]
listPosts = do
    p <- posts
    return $ fmap (uncurry getSummary) $ toList p

postServer :: Id -> Server PostAPI
postServer id = getPost id :<|> updatePost id :<|> deletePost id

getPost :: Id -> Handler Types.Post
getPost id = do
    p <- posts
    maybe (throwError err404) return (lookup id p)

updatePost :: Id -> Types.Post -> Handler ()
updatePost _ _ = throwError err500

deletePost :: Id -> Handler ()
deletePost _ = throwError err500
