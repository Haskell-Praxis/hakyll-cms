{-# LANGUAGE TypeFamilies #-}

module Hakyll.CMS.Server.Internal
    ()
    where

import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.Text
import           Hakyll.CMS.Types
import           Path
import           Path.IO

loadPost
  :: (MonadIO m, MonadReader m, EnvType m ~ (Path b Dir), MonadThrow m)
  => Id
  -> m Post
loadPost id = do
  dir      <- ask
  fileName <- parseRelFile (unpack id) >>= setFileExtension ".markdown"
  file     <- findFile [dir] fileName
  return undefined
