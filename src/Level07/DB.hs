{-# LANGUAGE OverloadedStrings #-}
module Level07.DB
  ( initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection)
import qualified Database.SQLite.Simple             as Sql

import qualified Common.SQLite.Error                as Sql
import           Common.SQLite.Error                (SQLiteResponse)

import           Level07.AppM                       (App, Env(..))
import           Level07.Types                      (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError),
                                                     FirstAppDB(..),
                                                     Topic, fromDBComment)
import Control.Monad.Error.Class (liftEither)


-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB db =
  Sql.close db.conn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp.getDBFilePath
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: App Connection
getDBConn = asks $ (.db.conn)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> App b
runDB f query = do
  conn <- getDBConn
  res <- liftIO $ Sql.runDBAction $ query conn
  liftEither $ either (Left . DBError) f res

getComments
  :: Topic
  -> App [Comment]
getComments topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    query conn = Sql.query conn sql (Sql.Only topic)
  in
    runDB (traverse fromDBComment) query

addCommentToTopic
  :: Topic
  -> CommentText
  -> App ()
addCommentToTopic topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    query conn = getCurrentTime >>= \time -> Sql.execute conn sql (topic, comment, time)
  in
    runDB Right query

getTopics
  :: App [Topic]
getTopics =
  let
    sql = "SELECT DISTINCT topic FROM comments"
    query conn = Sql.query_ conn sql :: IO [Topic]
  in
    runDB Right query

deleteTopic
  :: Topic
  -> App ()
deleteTopic topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
    query conn = Sql.execute conn sql (Sql.Only topic)
  in do
    runDB Right query

-- Go on to 'src/Level07/Core.hs' next.