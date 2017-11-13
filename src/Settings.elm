module Settings exposing (..)


type alias Settings =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    }


settings : Settings
settings =
    { left = 40
    , right = 100
    , top = 80
    , bottom = 40
    }
