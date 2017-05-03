{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Kubernetes.Pods

main :: IO ()
main = getLogsOf "theseus-online" "api-366374247-z9h38" "proxy" >>= \case
    Just x -> print x
