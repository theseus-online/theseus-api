{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Kubernetes.Services

main :: IO ()
main = getServicesOf "theseus-online" >>= print
