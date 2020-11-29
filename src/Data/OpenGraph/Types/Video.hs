module Data.OpenGraph.Types.Video (FilmData (..), Video (..)) where

import Data.OpenGraph.Types.NoVertical (HasTags (..), Profile)
import Data.Time (Day)

type Role = String

data FilmData = FilmData
    { actors :: [(Profile, Role)]
    , directors :: [Profile]
    , writers :: [Profile]
    , filmDuration :: Int
    , filmReleaseDate :: Day
    , filmDataTags :: [String]
    }
    deriving (Eq, Ord, Show)

instance HasTags FilmData where
    tags = filmDataTags

data Video
    = Movie FilmData
    | Episode FilmData FilmData
    | TVShow FilmData
    | Other FilmData
    deriving (Eq, Ord)
