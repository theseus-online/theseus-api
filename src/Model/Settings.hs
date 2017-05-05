module Model.Settings
    ( db
    , users
    , userinfoOf
    , volumeRoot
    ) where

import Kubernetes.Settings (volumeRoot)

db :: String
db = "http://db.theseus-online"

users :: String
users = db ++ "/users"

userinfoOf :: String -> String
userinfoOf username = db ++ "/users?name=eq." ++ username
