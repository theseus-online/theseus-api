{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Kubernetes.Services

main :: IO ()
main = getServices >>= print
