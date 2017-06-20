{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Cases
import           ClassyPrelude
import           Data.Map
import           Data.Time
import           Data.Time.Format
import           Hakyll.CMS.Server
import           Hakyll.CMS.Types         as Types
import           Network.Wai.Handler.Warp


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
    , Types.Post
        { Types.title = "Hello World"
        , author = "Sam Dent"
        , tags = ["tag1", "tag2"]
        , content = "Vestibulum sed placerat odio. Phasellus pulvinar ex in lorem auctor congue. Maecenas egestas auctor nisl, eget efficitur neque condimentum et. Ut auctor auctor molestie. Vestibulum porta urna sapien, eget iaculis velit bibendum sit amet. Pellentesque venenatis sapien in ligula egestas, eu aliquet orci laoreet."
        , date = createDate "2017-01-09"
        }
    , Types.Post
        { Types.title = "Getting Started"
        , author = "Sam Dent"
        , tags = ["tutorial", "tag2"]
        , content = "Donec commodo et mauris non tempor. Nam elementum, quam in dapibus bibendum, nisl tortor ornare elit, et viverra lorem erat non velit. Sed in vulputate lorem. Nulla eget metus eu velit pulvinar ullamcorper et a sem. Donec sodales ullamcorper enim sed molestie. Nullam gravida neque dui."
        , date = createDate "2017-01-10"
        }
    ]

getId :: Title -> UTCTime -> Text
getId title date =
    let titlePart = spinalize title
        datePart = pack $ formatTime defaultTimeLocale "%F" date
    in
        titlePart <> "_" <> datePart

posts :: Map Id Types.Post
posts =
    Data.Map.fromList $ fmap toPair postList
    where
        toPair post = (getId (Types.title post) (date post), post)

main :: IO ()
main = do
    state <- atomically $ newTVar posts
    run 8080 $ server state
