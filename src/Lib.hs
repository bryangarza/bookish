{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Lens
import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Network.Wreq

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Book = Book
  { bookUrl           :: T.Text
  , bookTitle         :: T.Text
  , bookSubtitle      :: Maybe T.Text
  , bookAuthors       :: Maybe [Author]
  , bookSubjects      :: Maybe [Subject]
  , bookPublishDate   :: Maybe String
  , bookCover         :: Maybe Cover
  , bookNumberOfPages :: Maybe Int
  , bookWeight        :: Maybe String
  } deriving (Generic, Show)

data Author = Author
  { authorUrl  :: T.Text
  , authorName :: T.Text
  } deriving (Generic, Show)

data Subject = Subject
  { subjectUrl  :: T.Text
  , subjectName :: T.Text
  } deriving (Generic, Show)

data Cover = Cover
  { coverSmall  :: T.Text
  , coverMedium :: T.Text
  , coverLarge  :: T.Text
  } deriving (Generic, Show)

data Books = Books [(T.Text, Book)] deriving (Show, Generic)

instance FromJSON Books where
  parseJSON = withObject "books" $
    return . Books . fold . HM.mapWithKey parseBook
    where parseBook isbn = foldMap pure . fmap (isbn,) . fromJSON

instance ToJSON Book where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance ToJSON Author where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance ToJSON Subject where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance ToJSON Cover where
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance FromJSON Book where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Author where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Subject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON Cover where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

type ISBN = String

type BooksResp = Response (Books)

books :: T.Text -> IO Books
books isbn = do
  r <- asJSON =<< getWith opts apiUrl
  return (r ^. responseBody)
    where apiUrl = "https://openlibrary.org/api/books?&format=json&jscmd=data"
          isbn' = [T.concat ["ISBN", isbn]]
          opts = defaults & param "bibkeys" .~ isbn'
