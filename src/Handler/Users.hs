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
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Model.Users as M
import Servant
    ( Get
    , JSON
    , Server
    , Proxy(..)
    , Capture
    , ReqBody
    , ServantErr
    , PutNoContent
    , NoContent(NoContent)
    , (:>)
    , err404
    , err500
    , errBody
    , throwError
    , (:<|>)((:<|>))
    )

type UsersAPI = "users" :> Get '[JSON] [M.User]
           :<|> "users" :> Capture "username" String :> Get '[JSON] M.User
           :<|> "users" :> Capture "username" String
                        :> ReqBody '[JSON] M.User
                        :> PutNoContent '[JSON] NoContent

usersServer :: Server UsersAPI
usersServer = getUsers :<|> getUser :<|> updateUser

getUsers :: ExceptT ServantErr IO [M.User]
getUsers = liftIO M.getUsers

getUser :: String -> ExceptT ServantErr IO M.User
getUser username = (liftIO (M.getUser username)) >>= \case
    Just u -> return u
    Nothing -> throwError $ err404 { errBody = "no such user" }

updateUser :: String -> M.User -> ExceptT ServantErr IO NoContent
updateUser _ userinfo =
    (liftIO $ M.updateUser userinfo) >>= \case
        Right _ -> return NoContent
        Left err -> throwError $ err500 { errBody = L.pack err }
