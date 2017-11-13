module Types exposing (..)


type alias Flags =
    { width : Int
    , height : Int
    }


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


type LengthUnit
    = Pixel
    | Normal
