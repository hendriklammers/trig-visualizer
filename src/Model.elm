module Model exposing (..)

import Vector exposing (Vector)
import Window exposing (Size)
import Messages exposing (..)


type alias Model =
    { windowSize : Size
    , triangle : Triangle
    }


type alias Triangle =
    { a : Vector
    , b : Vector
    , c : Vector
    }


initial : Model
initial =
    { windowSize = Size 0 0
    , triangle =
        { a = Vector 400 0
        , b = Vector 0 400
        , c = Vector 0 0
        }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )
