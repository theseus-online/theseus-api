{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Model.Ingresses

main :: IO ()
main = getIngressesOf "theseus-online" >>= print
