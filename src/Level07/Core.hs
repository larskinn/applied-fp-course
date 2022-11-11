{-# LANGUAGE OverloadedStrings #-}

module Level07.Core (
  runApplication,
  prepareAppReqs,
  app,
) where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (liftEither)

import Network.Wai (
  Application,
  Request,
  Response,
  pathInfo,
  requestMethod,
  strictRequestBody,
 )
import Network.Wai.Handler.Warp (run)

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Either (
  Either (Left, Right),
  either,
 )

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Common.SQLite.Error (SQLiteResponse)

import           Level07.AppM                       (App, Env(..), runApp)
import qualified Level07.Conf                       as Conf
import qualified Level07.DB                         as DB
import qualified Level07.Responses                  as Res
import           Level07.Types                      (Conf, ConfigError(..),
                                                     ContentType (..),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType, dbPath, getDBFilePath, confPortToWai)

-- | We're going to use the `mtl` ExceptT monad transformer to make the loading of
-- our `Conf` a bit more straight-forward.
import           Control.Monad.Except               (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import qualified System.IO as IO
import qualified Data.Text as T
import Control.Monad ( (<=<) )

-- | Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving Show

runApplication :: IO ()
runApplication = do
  appE <- runExceptT prepareAppReqs
  either print runWithDBConn appE
  where
    runWithDBConn env =
      appWithDB env >> DB.closeDB env.db

    appWithDB env = Ex.finally
      (run ( confPortToWai env.config ) (app env))
      $ DB.closeDB env.db

-- | Our AppM is no longer useful for implementing this function. Can you explain why?
--
-- We will reimplement this function using `ExceptT`. It is from the 'mtl'
-- package and it's the very general form of the AppM we implemented previously.
-- It has all of the useful instances written for us, along with many utility
-- functions.
--
-- 'mtl' on Hackage: https://hackage.haskell.org/package/mtl
--
prepareAppReqs :: ExceptT StartUpError IO Env
prepareAppReqs = do
  conf <- (liftEither <=< liftIO) $ first ConfErr <$> Conf.parseOptions "files/appconfig.json"
  db <- (liftEither <=< liftIO) $ first DBInitErr <$> DB.initDB conf.dbPath
  pure $ Env { loggingFn = logToConsole, config = conf, db = db}
  -- You may copy your previous implementation of this function and try refactoring it. On the
  -- condition you have to explain to the person next to you what you've done and why it works.

logToConsole :: Text -> App ()
logToConsole = liftIO . IO.putStr . T.unpack

-- | Now that our request handling and response creating functions operate
-- within our App context, we need to run the App to get our IO action out
-- to be run and handed off to the callback function. We've already written
-- the function for this so include the 'runApp' with the Env.
app
  :: Env
  -> Application
app env rq cb =
  -- cb . either mkErrorResponse id =<< runAppM (handleRequest =<< mkRequest rq) env
  do
    let getResponse = mkRequest rq >>= handleRequest
    response <- either mkErrorResponse id <$> runApp getResponse env
    cb response

handleRequest ::
  RqType ->
  App Response
handleRequest rqType = case rqType of
  AddRq topic comment -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic topic comment
  ViewRq topic        -> Res.resp200Json <$> DB.getComments topic
  ListRq              -> Res.resp200Json <$> DB.getTopics

mkRequest
  :: Request
  -> App RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO (mkAddRequest t <$> strictRequestBody rq)
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute     =
  Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       =
  Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ )    =
  -- Be a sensible developer and don't leak your DB errors over the internet.
  Res.resp500 PlainText "Database error"
