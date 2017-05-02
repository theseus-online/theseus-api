{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Users 
    ( getUser
    , getUsers
    , User(User)
    ) where

import GHC.Generics (Generic)
import Data.Aeson (decode)
import Control.Lens ((&), (.~), (^.), (^?), (^..))
import Data.Aeson.Types (ToJSON, FromJSON)
import Network.Wreq (get, defaults, header, responseBody)
import Model.Settings (users, userinfoOf)

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
