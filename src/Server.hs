{-# LANGUAGE OverloadedStrings #-}

module Server
  ( validateUserData
  ) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Aeson (FromJSON)
import Data.Monoid
import Data.Text
import Database.MySQL.Simple
import Web.Scotty

import Logger
import Types

validateData
  :: (FromJSON b)
  => Text -> ExceptT Text (WriterT Text ActionM) b
validateData url =
  (do egg <-
        lift . lift $ catchError (jsonData >>= (\x -> return . Just $ x)) (const . return $ Nothing)
      case egg of
        Just x -> do
          lift . tell $ "moshu moshi"
          return x
        _ -> do
          let message = "Inputs sent across to" <> url <> " are invalid"
          lift . tell $ message
          throwE message)

validateUserData :: ExceptT Text (WriterT Text ActionM) User
validateUserData = validateData "/login"

processUserData :: Connection -> User -> ExceptT Text (WriterT Text ActionM) (Maybe Int)
processUserData conn x = do
  lift . tell $ "querying the database for login information related to username" <> (username x)
  [Only i] <-
    liftIO $
    query conn "select id from userTables where username=? AND password=? " (username x, password x)
  return i
