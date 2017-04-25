{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Myself
    ( MyselfAPI
    , myselfServer
    ) where

import Control.Monad.IO.Class (liftIO)
import qualified Model.Users as M
import Servant (Get, JSON, Server, Proxy(..), Header, (:>), err500, errBody, throwError)

type MyselfAPI = "myself"
              :> Header "x-theseus-username" String
              :> Get '[JSON] M.User

myselfServer :: Server MyselfAPI
myselfServer maybeName = case maybeName of
    Just username -> (liftIO (M.getUser username)) >>= \case
        Just user -> return user
        Nothing -> throwError brokenGateWay -- TODO: report this, this is a critical error.
    Nothing -> throwError brokenGateWay     -- TODO: report this, this is a critical error.

    where
        brokenGateWay = err500 { errBody = "unauthorized user reaches api." }
