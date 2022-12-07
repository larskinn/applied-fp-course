{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Level07.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , FirstAppDB (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , confPortToWai
  , fromDBComment
  ) where

import           GHC.Generics               (Generic)
import           GHC.Word                   (Word16)
import           Data.ByteString            (ByteString)
import           Data.Semigroup             (Last (..), Semigroup ((<>)))
import           Data.Time                  (UTCTime)

import           Level07.DB.Types           (DBComment (..))
import           Level07.Types.CommentText  (CommentText, getCommentText, mkCommentText)
import           Level07.Types.Topic        (Topic, getTopic, mkTopic)
import           Level07.Types.Error        (Error (..))
import           Database.SQLite.Simple     (Connection)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson (ToJSON, FromJSON)
import Control.Exception (IOException)

newtype CommentId = CommentId Int
  deriving (Generic, Eq, Show)

instance ToJSON CommentId

-- | This is the `Comment` record that we will be sending to users, it's a
-- straightforward record type, containing an `Int`, `Topic`, `CommentText`, and
-- `UTCTime`.
data Comment = Comment
  { id    :: CommentId
  , topic :: Topic
  , body  :: CommentText
  , time  :: UTCTime
  }
  deriving (Generic, Show)

instance ToJSON Comment

-- | For safety we take our stored `DBComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  let
    commentId = CommentId dbc.id
    topic = mkTopic dbc.topic
    body = mkCommentText dbc.body
    time = dbc.time
  in
    Comment commentId <$> topic <*> body <*> pure time

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-----------------
-- Config Types
-----------------

-- This is an alternative way of defining a `newtype`. You define it as a record
-- with a single field, this provides the unwrapping function for free. When
-- defined using the other method, you must use pattern-matching or write a dedicated
-- function in order to get the value out.
--
newtype Port = Port
  -- You will notice we're using ``Word16`` as our type for the ``Port`` value.
  -- This is because a valid port number can only be a 16bit unsigned integer.
  { getPort :: Word16 }
  deriving (Eq, Show)
  deriving (FromJSON,ToJSON) via Word16 -- derive ToJSON, FromJSON, using the underlaying Word16 value


newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving stock (Eq, Show, Generic) -- specifying stock deriving
  deriving newtype (ToJSON,FromJSON) -- derive ToJSON, FromJSON, using the underllaying FilePath value


-- Add some fields to the ``Conf`` type:
-- - A customisable port number: ``Port``
-- - A filepath for our SQLite database: ``DBFilePath``
data Conf = Conf { port :: Port, dbPath :: DBFilePath }

-- We're storing our Port as a Word16 to be more precise and prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
--
-- We'll need to use a function called; ``fromIntegral``, to convert our
-- ``Word16`` to an ``Int``. The type of this function is:
--
-- fromIntegral :: (Num b, Integral a) => a -> b
--
confPortToWai
  :: Conf
  -> Int
confPortToWai conf = fromIntegral conf.port.getPort

-- Similar to when we were considering our application types. We can add to this sum type
-- as we build our application and the compiler can help us out.
data ConfigError
  = BadConfFile String
  | ConfigFileReadError IOException
  | MissingPortConf
  | MissingDbFileConf
  deriving Show


-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How do we combine the different
-- inputs to enable this property?

-- We want the command line configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- ``defaults <> file <> commandLine``

-- We can use the ``Semigroup`` typeclass to handle combining the ``Conf`` records
-- together, and the ``Last`` type to wrap up our values to handle the desired
-- precedence. The ``Last`` type is a wrapper for Maybe that when used with its
-- ``Semigroup`` instance will always preference the last value that it has:
--
-- Just (Last 3) <> Just (Last 1) = Just (Last 1)
-- Nothing       <> Just (Last 1) = Just (Last 1)
-- Just (Last 1) <> Nothing       = Just (Last 1)
--
-- To make this easier, we'll make a new type ``PartialConf`` that will have our ``Last``
-- wrapped values. We can then define a ``Semigroup`` instance for it and have our
-- ``Conf`` be a known good configuration.
data PartialConf = PartialConf
  { port       :: Maybe (Last Port)
  , dbFilePath :: Maybe (Last DBFilePath)
  } deriving (Generic)

instance FromJSON PartialConf

-- We need to define a ``Semigroup`` instance for ``PartialConf``. We define our ``(<>)``
-- function to lean on the ``Semigroup`` instance for Last to always get the last value.
instance Semigroup PartialConf where
  a <> b = PartialConf
    { port       = a.port <> b.port
    , dbFilePath = a.dbFilePath <> b.dbFilePath
    }

-- When it comes to reading the configuration options from the command-line, we
-- use the 'optparse-applicative' package. This part of the exercise has already
-- been completed for you, feel free to have a look through the 'CommandLine'
-- module and see how it works.

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { conn  :: Connection
  }

