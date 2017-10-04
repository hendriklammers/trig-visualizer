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
    , angleA : Float
    , angleB : Float
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
    , angleA = 0
    , angleB = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )

        UpdateTriangle triangle ->
            ( { model | triangle = triangle }
                |> calcLengths
                |> calcAngles
            , Cmd.none
            )


calcAngles : Model -> Model
calcAngles model =
    let
        t =
            model.triangle

        a =
            (model.lengthBC / model.lengthAC)
                |> atan
                |> radiansToDegrees

        b =
            (model.lengthAC / model.lengthBC)
                |> atan
                |> radiansToDegrees
    in
        { model | angleA = a, angleB = b }


radiansToDegrees : Float -> Float
radiansToDegrees radians =
    radians * 180 / pi


calcLengths : Model -> Model
calcLengths model =
    let
        t =
            model.triangle
    in
        { model
            | lengthAB = distance t.a t.b
            , lengthAC = distance t.a t.c
            , lengthBC = distance t.b t.c
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
