module Model exposing (..)

import Types exposing (..)
import Messages exposing (..)
import Debug


type alias Model =
    { triangle : Triangle
    , lengthAB : Float
    , lengthAC : Float
    , lengthBC : Float
    , angleA : Float
    , angleB : Float
    , normalize : Bool
    , drag : Bool
    }


initialTriangle : Triangle
initialTriangle =
    { a = Vector 0 0
    , b = Vector 400 400
    , c = Vector 0 400
    }


initial : Model
initial =
    { triangle = initialTriangle
    , lengthAB = 0
    , lengthAC = 0
    , lengthBC = 0
    , angleA = 0
    , angleB = 0
    , normalize = False
    , drag = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTriangle triangle ->
            updateTriangle model triangle

        ToggleNormalize ->
            ( { model | normalize = not model.normalize }, Cmd.none )

        DragStart pos ->
            ( { model | drag = True }, Cmd.none )

        DragAt pos ->
            let
                log =
                    Debug.log "pos" pos

                b =
                    { x = pos.x - 100, y = pos.y - 100 }

                triangle =
                    updateC <| { a = Vector 0 0, b = b, c = Vector 0 0 }
            in
                updateTriangle model triangle

        DragEnd pos ->
            ( { model | drag = False }, Cmd.none )


updateTriangle : Model -> Triangle -> ( Model, Cmd Msg )
updateTriangle model triangle =
    ( { model | triangle = updateC triangle }
        |> calcLengths
        |> calcAngles
    , Cmd.none
    )


updateC : Triangle -> Triangle
updateC triangle =
    { triangle | c = Vector triangle.a.x triangle.b.y }


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
