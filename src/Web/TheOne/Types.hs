module Web.TheOne.Types
  ( AccessToken(..)
  , TheOneEnv (..)
  , TheOneError (..)
  , Entities (..)
  , Entity (..)
  , Book (..)
  , Movie (..)
  , Character (..)
  , Quote (..)
  , Chapter (..)
  , toTheOneError
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, splitOn, toTitle)
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client (Manager)
import Servant.Client.Core

newtype AccessToken = AccessToken
  { unAccessToken :: String
  }

data TheOneEnv = TheOneEnv
  { theOneManager :: Manager
  , theOneAccessToken :: AccessToken
  }

newtype TheOneError = TheOneError
  { theOneError :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON TheOneError where
  toJSON = theOneToJSON 6

instance FromJSON TheOneError where
  parseJSON = theOneParseJSON 6

toTheOneError :: ClientError -> TheOneError
toTheOneError (DecodeFailure _ r) = fromMaybe defaultError . decode $ responseBody r
toTheOneError (FailureResponse _ r) = fromMaybe defaultError . decode $ responseBody r
toTheOneError (UnsupportedContentType _ r) = fromMaybe defaultError . decode $ responseBody r
toTheOneError (InvalidContentTypeHeader r) = fromMaybe defaultError . decode $ responseBody r
toTheOneError _ = defaultError

-- TODO: This really should be more useful than this
defaultError :: TheOneError
defaultError = TheOneError "Unknown error"

data Entities a = Entities
  { entitiesDocs :: [a]
  , entitiesTotal :: Integer
  , entitiesLimit :: Integer
  , entitiesOffset :: Maybe Integer
  , entitiesPage :: Maybe Integer
  , entitiesPages :: Maybe Integer
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Entities a) where
  toJSON = theOneToJSON 8
instance FromJSON a => FromJSON (Entities a) where
  parseJSON = theOneParseJSON 8

newtype Entity a = Entity
  { resultEntity :: a
  } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Entity a) where
  toJSON obj = undefined
instance FromJSON a => FromJSON (Entity a) where
  parseJSON (Object o) = do
    [v] <- o .: "docs"
    pure $ Entity v
  parseJSON _ = fail "Failed to parse Entity object"

data Book = Book
  { book_id :: Text
  , bookName :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Book where
  toJSON = theOneToJSON 4
instance FromJSON Book where
  parseJSON = theOneParseJSON 4

data Movie = Movie
  { movie_id :: Text
  , movieName :: Text
  , movieRuntimeInMinutes :: Integer
  , movieBudgetInMillions :: Double
  , movieBoxOfficeRevenueInMillions :: Double
  , movieAcademyAwardNominations :: Integer
  , movieAcademyAwardWins :: Integer
  , movieRottenTomatoesScore :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON Movie where
  toJSON = theOneToJSON 5
instance FromJSON Movie where
  parseJSON = theOneParseJSON 5

data Character = Character
  { character_id :: Text
  , characterBirth :: Text
  , characterDeath :: Text
  , characterHair :: Text
  , characterGender :: Maybe Text
  , characterHeight :: Text
  , characterRealm :: Text
  , characterSpouse :: Text
  , characterName :: Text
  , characterRace :: Text
  , characterWikiUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Character where
  toJSON = theOneToJSON 9
instance FromJSON Character where
  parseJSON = theOneParseJSON 9

data Quote = Quote
  { quote_id :: Text
  , quoteDialog :: Text
  , quoteMovie :: Text
  , quoteCharacter :: Text
  , quoteId :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Quote where
  toJSON = theOneToJSON 5
instance FromJSON Quote where
  parseJSON = theOneParseJSON 5

data Chapter = Chapter
  { chapter_id :: Text
  , chapterChapterName :: Text
  , chapterBook :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Chapter where
  toJSON = theOneToJSON 7
instance FromJSON Chapter where
  parseJSON = theOneParseJSON 7

-- JSON helpers
theOneParseJSON
  :: (GFromJSON Zero (Rep a), Generic a)
  => Int
  -> Value
  -> Parser a
theOneParseJSON i = genericParseJSON defaultOptions
  { fieldLabelModifier = drop i } . toCapitalizeJson . unSnakeCaseJson

theOneToJSON
  :: (GToJSON Zero (Rep a), Generic a)
  => Int
  -> a
  -> Value
theOneToJSON i = genericToJSON defaultOptions
  { fieldLabelModifier = snakeCase . lowerCaseFirst . drop i
  , omitNothingFields = True
  }

capitalizeT :: Key -> Key
capitalizeT = K.fromText . T.foldl foldHelper "" . K.toText
  where
    foldHelper :: Text -> Char -> Text
    foldHelper a x = a <> if a == ""
      then pack [C.toUpper x]
      else pack [x]

lowerCaseFirst :: String -> String
lowerCaseFirst []     = ""
lowerCaseFirst [x]    = [C.toLower x]
lowerCaseFirst (x:xs) = [C.toLower x] <> xs

snakeCase :: String -> String
snakeCase [] = ""
snakeCase xs = foldl foldHelper "" xs
  where
    foldHelper :: String -> Char -> String
    foldHelper a x = a <> if C.isUpper x
      then ['_', C.toLower x]
      else [x]

unSnakeCase :: Key -> Key
unSnakeCase t =
    if (T.isInfixOf "_id" txt)
      then K.fromText "_id"
      else K.fromText $ convertHelper (splitOn "_" txt)
  where
    txt :: Text
    txt = K.toText t

    convertHelper :: [Text] -> Text
    convertHelper []     = ""
    convertHelper (x:xs) = x <> foldl foldHelper "" xs

    foldHelper :: Text -> Text -> Text
    foldHelper a x = a <> toTitle x

unSnakeCaseJson :: Value -> Value
unSnakeCaseJson = transformJsonKey $ first unSnakeCase

transformJsonKey :: ((Key, Value) -> (Key, Value)) -> Value -> Value
transformJsonKey func (Object o) = Object . KM.fromList . map func . KM.toList $ o
transformJsonKey _ x = x

toCapitalizeJson :: Value -> Value
toCapitalizeJson = transformJsonKey $ first capitalizeT
