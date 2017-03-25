{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Myself 
    ( MyselfAPI
    , myselfServer
    ) where

import Model.Users (User(..))
import Servant (Get, JSON, Server, Proxy(..), Header, (:>), err400, errBody, throwError)

type MyselfAPI = "myself"
              :> Header "name" String
              :> Header "email" String
              :> Header "avatar" String
              :> Get '[JSON] User

myselfServer :: Server MyselfAPI
myselfServer maybeName maybeEmail maybeAvatar = case maybeUser of
    Just user -> return user
    Nothing -> throwError badRequest -- TODO: report this.

    where maybeUser = maybeName 
                  >>= \name -> maybeEmail
                  >>= \email -> maybeAvatar
                  >>= \avatar -> Just $ User name email avatar

          badRequest = err400 { errBody = "one of 'name', 'email', 'avatar' not shown." }