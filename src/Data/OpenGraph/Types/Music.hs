module Data.OpenGraph.Types.Music (Song (..), Album (..), HasMusicians (..), HasSongs(..)) where

import Data.OpenGraph.Types.NoVertical (Authored(..), Profile)
import Data.Time (Day)

type Disc = Int
type Track = Int

class HasMusicians a where
    musicians :: a -> [Profile]

class HasSongs a where
    songs :: a -> [(Song, Disc, Track)]

data Song = Song
    { songMusicians :: [Profile]
    , albums :: [(Album, Disc, Track)]
    , songDuration :: Int
    }
    deriving (Eq, Ord, Show)

instance HasMusicians Song where
    musicians = songMusicians

data Album = Album
    { albumSongs :: [(Song, Disc, Track)]
    , albumMusicians :: [Profile]
    , albumReleaseDate :: Day
    }
    deriving (Eq, Ord, Show)

instance HasMusicians Album where
    musicians = albumMusicians

instance HasSongs Album where
    songs = albumSongs

data Playlist = Playlist
    { playlistSongs :: [(Song, Disc, Track)]
    , playlistCreator :: Profile
    }
    deriving (Eq, Ord, Show)

instance Authored Playlist where
    author = playlistCreator

instance HasSongs Playlist where
    songs = playlistSongs

newtype RadioStation = RadioStation {stationCreator :: Profile} deriving (Eq, Ord, Show)

instance Authored RadioStation where
    author = stationCreator