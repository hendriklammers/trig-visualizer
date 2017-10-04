module Model exposing (..)

import Types exposing (Vector, Triangle)
import Window exposing (Size)
import Messages exposing (..)
import Debug


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
            { model | windowSize = size } ! []

        UpdateTriangle triangle ->
            calcLengths triangle model ! []


calcLengths : Triangle -> Model -> Model
calcLengths triangle model =
    let
        log =
            Debug.log "triangle" triangle
    in
        { model
            | lengthAB = distance triangle.a triangle.b
            , lengthAC = distance triangle.a triangle.c
            , lengthBC = distance triangle.b triangle.c
            , triangle = triangle
        }


distance : Vector -> Vector -> Float
distance v1 v2 =
    let
        x =
            toFloat <| v1.x - v2.x

        y =
            toFloat <| v1.y - v2.y
    in
        sqrt (x * x + y * y)
