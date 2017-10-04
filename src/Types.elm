module Types exposing (..)


type alias Vector =
    { x : Int
    , y : Int
    }


type alias Triangle =
    { a : Vector
    , b : Vector
    , c : Vector
    }
