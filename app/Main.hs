{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Server
import Types
import Web.Scotty
import Database.MySQL.Simple

routes :: Connection -> ScottyM ()
routes conn = post "/" do
  (x, logs) <- (runWriterT . runExceptT $ )
  putStrLn logs

main :: IO ()
main = scotty 8000 routes
