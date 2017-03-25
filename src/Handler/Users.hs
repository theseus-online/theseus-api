{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Users 
    ( UsersAPI
    , usersServer
    ) where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Model.Users as M
import Servant (Get, JSON, Server, Proxy(..), Capture, ServantErr, (:>), err404, errBody, throwError, (:<|>)((:<|>)))

type UsersAPI = "users" :> Get '[JSON] [M.User]
           :<|> "users" :> Capture "username" String :> Get '[JSON] M.User

usersServer :: Server UsersAPI
usersServer = getUsers :<|> getUser

getUsers :: ExceptT ServantErr IO [M.User]
getUsers = liftIO M.getUsers

getUser :: String -> ExceptT ServantErr IO M.User
getUser username = (liftIO (M.getUser username)) >>= \case
    Just u -> return u
    Nothing -> throwError $ err404 { errBody = "no such user" }