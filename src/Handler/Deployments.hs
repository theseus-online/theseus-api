{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Deployments
    ( DeploymentsAPI
    , deploymentsServer
    ) where

import qualified Model.Deployments as M
import Theseus.User (User(..))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Servant (Get, JSON, Server, Proxy(..), Capture, (:>), err500, errBody, throwError)

type DeploymentsAPI = "users"
                   :> Capture "username" String
                   :> "deployments"
                   :> Get '[JSON] [M.Deployment]

deploymentsServer :: Server DeploymentsAPI
deploymentsServer username = do
    r <- liftIO $ M.getDeploymentsOf username
    case r of
        Right deps -> return deps
        Left err -> throwError $ err500 { errBody = pack err }