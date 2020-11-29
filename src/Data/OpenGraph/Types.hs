module Data.OpenGraph.Types (
    module Types,
) where

import Data.List.Extra (sort)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.OpenGraph.Types.Music as Types
import Data.OpenGraph.Types.NoVertical as Types
import Data.OpenGraph.Types.Video as Types
import Data.Time (Day, LocalTime (localDay), defaultTimeLocale, parseTimeOrError)
import Data.Tuple.Extra (fst3)

class HasDuration a where
    duration :: a -> Int

instance HasDuration Song where
    duration = songDuration

instance HasDuration FilmData where
    duration = filmDuration

instance HasDuration Video where
    duration =
        duration . \case
            Movie fd -> fd
            Episode fd _ -> fd
            TVShow fd -> fd
            Other fd -> fd

class HasReleaseDate a where
    releaseDate :: a -> Day

instance HasReleaseDate Book where
    releaseDate = bookReleaseDate

instance HasReleaseDate Article where
    releaseDate = localDay . published

instance HasReleaseDate Song where
    releaseDate Song{..} = fromMaybe (parseTimeOrError False defaultTimeLocale "%F" "1900-01-01") . listToMaybe . sort $ map (releaseDate . fst3) albums

instance HasReleaseDate Album where
    releaseDate Album{..} = albumReleaseDate

instance HasReleaseDate FilmData where
    releaseDate = filmReleaseDate

instance HasReleaseDate Video where
    releaseDate =
        releaseDate . \case
            Movie fd -> fd
            Episode fd _ -> fd
            TVShow fd -> fd
            Other fd -> fd