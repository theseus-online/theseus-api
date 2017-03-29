{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Kubernetes.Deployments

main :: IO ()
main = do
    createDeployment (Deployment "n" "n" [])
