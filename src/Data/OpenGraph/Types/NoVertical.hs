module Data.OpenGraph.Types.NoVertical (
    Gender (..),
    Profile (..),
    fullName,
    Authored (..),
    HasTags (..),
    Book (..),
    Article (..),
    Website
) where

import Data.Char (toLower)
import Data.Time (Day, LocalTime)
import Text.HTML.Scalpel (URL)
import Data.List.Extra (trim)

data Gender
    = Unspecified
    | Male
    | Female
    | Nonbinary
    deriving (Eq, Ord, Enum, Bounded)

instance Show Gender where
    show = \case
        Unspecified -> "unspecified"
        Female -> "female"
        Male -> "male"
        Nonbinary -> "non-binary"

instance Read Gender where
    readsPrec _ =
        pure . (,[])
            . ( \case
                    "female" -> Female
                    "male" -> Male
                    "non-binary" -> Nonbinary
                    "nonbinary" -> Nonbinary
                    _ -> Unspecified
              )
            . map toLower
            . trim

type FirstName = String
type LastName = String
type Username = String

data Profile = Profile
    { firstName :: FirstName
    , lastName :: LastName
    , username :: Username
    , gender :: Gender
    }
    deriving (Eq, Ord, Show)

fullName :: Profile -> String
fullName Profile{..} = firstName ++ ' ' : lastName

class Authored a where
    author :: a -> Profile

class HasTags a where
    tags :: a -> [String]

data Book = Book
    { bookAuthor :: Profile
    , isbn :: String
    , bookReleaseDate :: Day
    , bookTags :: [String]
    }
    deriving (Eq, Ord, Show)

instance Authored Book where
    author = bookAuthor

instance HasTags Book where
    tags = bookTags

data Article = Article
    { published :: LocalTime
    , lastModified :: LocalTime
    , expirationTime :: LocalTime
    , articleAuthor :: Profile
    , section :: String
    , articleTags :: [String]
    }
    deriving (Eq, Ord, Show)

instance Authored Article where
    author = articleAuthor

instance HasTags Article where
    tags = articleTags

type Website = URL