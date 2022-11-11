{-# LANGUAGE OverloadedStrings #-}
module Level07.Types.Error
  ( Error(..)
  , nonEmptyText
  ) where

import Data.Text (Text)
import Common.SQLite.Error (SQLiteResponse)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError SQLiteResponse
  deriving (Eq, Show)

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
