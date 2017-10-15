module Types exposing (..)

import Mouse exposing (Position)


type alias Vector =
    { x : Int
    , y : Int
    }


type alias Triangle =
    { a : Vector
    , b : Vector
    , c : Vector
    }


type alias Length =
    { position : Vector
    , rotation : Float
    , value : Float
    }


type alias Drag =
    { start : Position
    , current : Position
    }
