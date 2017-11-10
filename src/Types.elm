module Types exposing (..)


type alias Position =
    { x : Int
    , y : Int
    }


type alias Triangle =
    { a : Position
    , b : Position
    , c : Position
    }


type alias Length =
    { position : Position
    , rotation : Float
    , value : Float
    }


type alias Drag =
    { offset : Position
    }


type Unit
    = Pixel
    | Normal
