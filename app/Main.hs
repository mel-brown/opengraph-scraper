{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.OpenGraph (openGraphScraper)

import Control.Monad.IO.Class (liftIO)
import Happstack.Server (
    Response,
    ServerPart,
    ToMessage (toResponse),
    look,
    nullConf,
    ok,
    simpleHTTP,
 )
import Text.Blaze.Html5 (Html, h1, p, toHtml)
import qualified Text.Blaze.Html5 as H
import Text.HTML.Scalpel (scrapeURL)

main :: IO ()
main = simpleHTTP nullConf test

template :: String -> Html -> Response
template title body = toResponse $
    H.html $ do
        H.head $ H.title (toHtml title)
        H.body body

test :: ServerPart Response
test = do
    url <- look "url"
    urlData <- liftIO (scrapeURL url openGraphScraper)
    ok $
        template "OpenGraph Scraper" $ do
            h1 "OpenGraph Data"
            mconcat . map (p . toHtml) . lines . maybe "OpenGraph data not found!" show $ urlData
