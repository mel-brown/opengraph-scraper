{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.OpenGraph

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Maybe
import           Happstack.Server
import           Text.HTML.Scalpel
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 (Html, toHtml, h1, p)

main :: IO ()
main = simpleHTTP nullConf test

template :: String -> Html -> Response
template title body = toResponse $
    H.html $ do
        H.head $ H.title (toHtml title)
        H.body body

test :: ServerPart Response
test = do
    url     <- look "url"
    urlData <- liftIO (scrapeURL url openGraphScraper)
    ok $ template "OpenGraph Scraper" $ do
        h1 "OpenGraph Data"
        mconcat . map (p . toHtml) . lines . maybe "OpenGraph data not found!" show $ urlData
