{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Model.Volumes

main :: IO ()
main = getVolumes >>= print
