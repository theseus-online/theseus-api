{-# LANGUAGE DeriveGeneric #-}

module Theseus.User (
    User(User)
    ) where

import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON)

data User = User
    { name :: String
    , email :: String
    , avatar :: String
    } deriving (Eq, Show, Generic)

instance ToJSON User