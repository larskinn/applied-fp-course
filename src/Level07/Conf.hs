{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Level07.Conf
    ( parseOptions
    ) where

import           GHC.Word                 (Word16)

import           Level07.Types            (Conf (..), ConfigError (MissingPortConf, MissingDbFileConf),
                                           DBFilePath (DBFilePath), PartialConf (..),
                                           Port (Port))

import           Level07.Conf.CommandLine (commandLineParser)
import           Level07.Conf.File        (parseJSONConfigFile)
import Data.Semigroup (Last(Last), getLast)

-- | For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf
  { port = Just $ Last $ Port 3000
  , dbFilePath = Just $ Last $ DBFilePath ":memory:"
  }

-- | We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc =
  Conf
    <$> getFieldOr MissingPortConf (.port)
    <*> getFieldOr MissingDbFileConf (.dbFilePath)
  where
    getFieldOr err f = maybe (Left err) (Right . getLast) $ f pc

-- | This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
--
-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions path = do
  -- Parse the options from the config file: "files/appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  let withDefaultConf file commandLine = defaultConf <> file <> commandLine
  fileConfig <- parseJSONConfigFile path
  cmdLineConfig <- commandLineParser
  pure $ makeConfig =<< withDefaultConf <$> fileConfig <*> pure cmdLineConfig
