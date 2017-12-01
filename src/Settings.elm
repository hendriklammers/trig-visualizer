module Settings exposing (Settings, settings)


type alias Settings =
    { left : Int
    , right : Int
    , top : Int
    , bottom : Int
    , gridSize : Int
    }


settings : Settings
settings =
    { left = 40
    , right = 100
    , top = 80
    , bottom = 40
    , gridSize = 10
    }
