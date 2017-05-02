{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Model.Deployments

main :: IO ()
main = getDeploymentsOf "lucklove" >>= print
