module Model.Settings
    ( users
    , userinfoOf
    ) where

db :: String
db = "http://db.theseus-online"


users :: String
users = db ++ "/users"

userinfoOf :: String -> String
userinfoOf username = db ++ "/users?name=eq." ++ username