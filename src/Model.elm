module Model exposing (..)

import Types exposing (..)
import Messages exposing (..)


type alias Model =
    { triangle : Triangle
    , lengthAB : Float
    , lengthAC : Float
    , lengthBC : Float
    , angleA : Float
    , angleB : Float
    , normalize : Bool
    , drag : Maybe Drag
    }


initialTriangle : Triangle
initialTriangle =
    { a = Position 0 0
    , b = Position 400 400
    , c = Position 0 400
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
    , drag = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTriangle triangle ->
            updateTriangle model triangle

        ToggleNormalize ->
            ( { model | normalize = not model.normalize }, Cmd.none )

        DragStart pos ->
            let
                log =
                    Debug.log "pos" pos

                offset =
                    { x = model.triangle.b.x - pos.x + 100
                    , y = model.triangle.b.y - pos.y + 100
                    }
            in
                ( { model | drag = Just (Drag offset) }, Cmd.none )

        DragAt pos ->
            case model.drag of
                Just { offset } ->
                    let
                        x =
                            (pos.x - 100) + offset.x

                        y =
                            (pos.y - 100) + offset.y

                        b =
                            { x = limitInt <| x
                            , y = limitInt <| y
                            }

                        triangle =
                            updateC <| { a = Position 0 0, b = b, c = Position 0 0 }
                    in
                        updateTriangle model triangle

                Nothing ->
                    model ! []

        DragEnd pos ->
            ( { model | drag = Nothing }, Cmd.none )


limitInt : Int -> Int
limitInt n =
    if n < 30 then
        30
    else
        n


updateTriangle : Model -> Triangle -> ( Model, Cmd Msg )
updateTriangle model triangle =
    ( { model | triangle = updateC triangle }
        |> calcLengths
        |> calcAngles
    , Cmd.none
    )


updateC : Triangle -> Triangle
updateC triangle =
    { triangle | c = Position triangle.a.x triangle.b.y }


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


distance : Position -> Position -> Float
distance v1 v2 =
    let
        x =
            toFloat <| v1.x - v2.x

        y =
            toFloat <| v1.y - v2.y
    in
        sqrt (x * x + y * y)
