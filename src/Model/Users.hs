{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Users
    ( getUser
    , getUsers
    , updateUser
    , User(User)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (decode)
import qualified Kubernetes.Namespaces as KN
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Data.Aeson.Types (ToJSON, FromJSON)
import Model.Settings (db, users, userinfoOf)
import Network.Wreq (get, post, defaults, header, responseBody, FormParam((:=)))

data User = User
    { name :: String
    , email :: String
    , avatar :: String
    } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

getUser :: String -> IO (Maybe User)
getUser username = do
    r <- get $ userinfoOf username
    return $ decode (r ^. responseBody) >>= \case
        Just (u:us) -> u
        _ -> Nothing

getUsers :: IO [User]
getUsers = do
    r <- get users
    case decode (r ^. responseBody) of
        Just us -> return us
        Nothing -> return []

updateUser :: User -> IO (Either String ())
updateUser (User n e a) = do
    firstLogin <- getUser n >>= \case
        Just _ -> return False
        Nothing -> return True
    post (db ++ "/rpc/create_or_update_user") [ "name" := n
                                              , "email" := e
                                              , "avatar" := a
                                              ]
    if firstLogin
        then KN.initNamespace n >>= \case
            Right _ -> return $ Right ()
            e -> return e
        else return $ Right ()
