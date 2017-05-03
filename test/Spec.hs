{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Model.Deployments

main :: IO ()
main = getDeployments >>= print
