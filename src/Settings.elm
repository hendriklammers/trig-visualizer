module Settings exposing (..)


type alias Settings =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    , width : Int
    , height : Int
    }


settings : Settings
settings =
    { left = 40
    , right = 80
    , top = 80
    , bottom = 40
    , width = 800
    , height = 600
    }
