{-# LANGUAGE OverloadedStrings #-}

module Level04.Core (
  runApp,
  prepareAppReqs,
  app,
) where

import Control.Applicative (liftA2)
import Control.Monad (join)

import Network.Wai (
  Application,
  Request,
  Response,
  pathInfo,
  requestMethod,
  responseLBS,
  strictRequestBody,
 )
import Network.Wai.Handler.Warp (run)

import Network.HTTP.Types (
  Status,
  hContentType,
  status200,
  status400,
  status404,
  status500,
 )

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Either (
  Either (Left, Right),
  either,
 )

import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Common.SQLite.Error (SQLiteResponse)

import qualified Data.Aeson as Aeson

import Data.Aeson (encode)
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson.Types (ToJSON)
import Data.Bifunctor
import Level04.Conf (Conf (Conf, dbFilePath), firstAppConfig)
import Level04.DB
import qualified Level04.DB as DB
import Level04.Types (
  ContentType (JSON, PlainText),
  Error (..),
  RqType (AddRq, ListRq, ViewRq),
  mkCommentText,
  mkTopic,
  renderContentType,
 )

import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (stderr, hPrint)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  deriving (Show)

runApp :: IO ()
runApp = do
  res <- prepareAppReqs firstAppConfig
  either exitWithErrorMessage (run 3000 . app) res

exitWithErrorMessage :: StartUpError -> IO ()
exitWithErrorMessage (DBInitErr sqlErr) = do
  hPrint stderr sqlErr
  exitWith $ ExitFailure 1

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
--
-- Our application configuration is defined in Conf.hs
--
prepareAppReqs ::
  Conf -> IO (Either StartUpError DB.FirstAppDB)
prepareAppReqs conf =
  first DBInitErr <$> initDB conf.dbFilePath


-- | Some helper functions to make our lives a little more DRY.
mkResponse ::
  Status ->
  ContentType ->
  LBS.ByteString ->
  Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp200 =
  mkResponse status200

resp404 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp404 =
  mkResponse status404

resp400 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp400 =
  mkResponse status400

-- Some new helpers for different statuses and content types
resp500 ::
  ContentType ->
  LBS.ByteString ->
  Response
resp500 =
  mkResponse status500

resp200Json ::
  ToJSON a =>
  a ->
  Response
resp200Json =
  mkResponse status200 JSON . encode

-- |
app ::
  -- | Add the Database record to our app so we can use it
  DB.FirstAppDB ->
  Application
app db rq cb = do
  rq' <- mkRequest rq
  resp <- handleRespErr <$> handleRErr rq'
  cb resp
 where
  handleRespErr :: Either Error Response -> Response
  handleRespErr = either mkErrorResponse id

  -- We want to pass the Database through to the handleRequest so it's
  -- available to all of our handlers.
  handleRErr :: Either Error RqType -> IO (Either Error Response)
  handleRErr = either (pure . Left) (handleRequest db)

{- | Handle each of the different types of request. See how the types have helped narrow our focus
 to only those types of request that we care about. Along with ensuring that once the data has
 reached this point, we don't have to continually check if it is valid or usable. The types and
 data structures that we created have taken care of that for us at an earlier stage, simplifying
 this function.

 For both the 'ViewRq' and 'ListRq' functions, we'll need to pass the correct 'Encoder' to the
 'resp200Json' function.
-}
handleRequest ::
  DB.FirstAppDB ->
  RqType ->
  IO (Either Error Response)
handleRequest _db (AddRq topic comment) =
  (resp200 PlainText "Success" <$) <$> addCommentToTopic _db topic comment
handleRequest _db (ViewRq topic) =
  fmap resp200Json <$> getComments _db topic
handleRequest _db ListRq =
  fmap resp200Json <$> getTopics _db

mkRequest ::
  Request ->
  IO (Either Error RqType)
mkRequest rq =
  case (pathInfo rq, requestMethod rq) of
    -- Commenting on a given topic
    ([t, "add"], "POST") -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ([t, "view"], "GET") -> pure (mkViewRequest t)
    -- List the current topics
    (["list"], "GET") -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _ -> pure (Left UnknownRoute)

mkAddRequest ::
  Text ->
  LBS.ByteString ->
  Either Error RqType
mkAddRequest ti c =
  AddRq
    <$> mkTopic ti
    <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest ::
  Text ->
  Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest ::
  Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse ::
  Error ->
  Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
mkErrorResponse (SqlError err) =
  resp500 PlainText $ LBS.pack $ "SQL Error: " ++ show err
