{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Myself 
    ( myselfAPI
    , myselfServer
    ) where

import Theseus.User (User(..))
import Servant (Get, JSON, Server, Proxy(..), Header, (:>), err400, errBody, throwError)

type MyselfAPI = "myself"
             :> Header "username" String
             :> Header "email" String
             :> Header "avatar" String
             :> Get '[JSON] User

myselfAPI :: Proxy MyselfAPI
myselfAPI = Proxy

myselfServer :: Server MyselfAPI
myselfServer maybeName maybeEmail maybeAvatar = case maybeUser of
    Just user -> return user
    Nothing -> throwError badRequest -- TODO: report this.

    where maybeUser = maybeName 
                  >>= \name -> maybeEmail
                  >>= \email -> maybeAvatar
                  >>= \avatar -> Just $ User name email avatar

          badRequest = err400 { errBody = "one of 'name', 'email', 'avatar' not shown." }