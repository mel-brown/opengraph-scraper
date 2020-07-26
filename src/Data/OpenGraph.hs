{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.OpenGraph
    ( OpenGraph(..)
    , openGraphScraper
    ) where

import Data.Maybe
import GHC.Generics
import Text.HTML.Scalpel

data OpenGraph = OG { ogTitle :: String
                    , ogURL   :: URL
                    , ogImage :: URL
                    , ogType  :: Maybe String
                    , ogDesc  :: Maybe String
                    } deriving (Generic, Eq)

instance Show OpenGraph where
    show OG{..} =
        "{\n\"og:title\": " ++ show ogTitle ++
        ",\n\"og:url\": "   ++ show ogURL ++
        ",\n\"og:image\": " ++ show ogImage ++
        (if isNothing ogType then "" else ",\n\"og:type\": "  ++ show (fromJust ogType)) ++
        (if isNothing ogDesc then "" else ",\n\"og:description\": " ++ show (fromJust ogDesc)) ++
        "\n}"

openGraphScraper :: Scraper String OpenGraph
openGraphScraper = do
    title <- attrs "content" $ "meta" @: ["property" @= "og:title"]
    url   <- attrs "content" $ "meta" @: ["property" @= "og:url"]
    img   <- attrs "content" $ "meta" @: ["property" @= "og:image"]
    otype <- attrs "content" $ "meta" @: ["property" @= "og:type"]
    desc  <- attrs "content" $ "meta" @: ["property" @= "og:description"]
    return $ OG (head title) (head url) (head img) (listToMaybe otype) (listToMaybe desc)