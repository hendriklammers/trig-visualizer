module Model exposing (..)

import Types exposing (Vector, Triangle)
import Window exposing (Size)
import Messages exposing (..)


type alias Model =
    { windowSize : Size
    , triangle : Triangle
    , lengthAB : Float
    , lengthAC : Float
    , lengthBC : Float
    }


initial : Model
initial =
    { windowSize = Size 0 0
    , triangle =
        { a = Vector 400 0
        , b = Vector 0 400
        , c = Vector 0 0
        }
    , lengthAB = 0
    , lengthAC = 0
    , lengthBC = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )

        UpdateTriangle triangle ->
            model ! []
