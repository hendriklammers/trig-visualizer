module Model exposing (Model, update)

import Types exposing (..)
import Messages exposing (..)
import Settings exposing (settings)


type alias Model =
    { triangle : Triangle
    , lengthAB : Float
    , lengthAC : Float
    , lengthBC : Float
    , angleA : Float
    , angleB : Float
    , cosA : Float
    , sinA : Float
    , cosB : Float
    , sinB : Float
    , drag : Maybe Drag
    , unit : LengthUnit
    , width : Int
    , height : Int
    , shiftDown : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTriangle triangle ->
            updateTriangle model triangle

        ChangeLengthUnit unit ->
            ( { model | unit = unit }, Cmd.none )

        DragStart pos ->
            let
                offset =
                    { x = model.triangle.b.x - pos.x + settings.left
                    , y = model.triangle.b.y - pos.y + settings.top
                    }
            in
                ( { model | drag = Just (Drag offset) }, Cmd.none )

        DragAt pos ->
            case model.drag of
                Just { offset } ->
                    let
                        x =
                            (pos.x - settings.left) + offset.x

                        y =
                            (pos.y - settings.top) + offset.y

                        maxX =
                            (model.width - settings.left - settings.right)

                        maxY =
                            (model.height - settings.top - settings.bottom)

                        b =
                            { x = limitPosition model.shiftDown maxX x
                            , y = limitPosition model.shiftDown maxY y
                            }

                        triangle =
                            updateC <|
                                { a = Position 0 0
                                , b = b
                                , c = Position 0 0
                                }
                    in
                        updateTriangle model triangle

                Nothing ->
                    model ! []

        DragEnd pos ->
            ( { model | drag = Nothing }, Cmd.none )

        KeyDown keycode ->
            ( { model | shiftDown = keycode == 16 }, Cmd.none )

        KeyUp keycode ->
            if keycode == 16 then
                ( { model | shiftDown = False }, Cmd.none )
            else
                model ! []


limitPosition : Bool -> Int -> Int -> Int
limitPosition snap max n =
    if n < 0 then
        0
    else if n > max then
        max
    else if snap then
        snapToGrid n
    else
        n


snapToGrid : Int -> Int
snapToGrid n =
    (round <| toFloat n / toFloat settings.gridSize) * settings.gridSize


updateTriangle : Model -> Triangle -> ( Model, Cmd Msg )
updateTriangle model triangle =
    ( { model | triangle = updateC triangle }
        |> calcLengths
        |> calcAngles
        |> calcTrig
    , Cmd.none
    )


updateC : Triangle -> Triangle
updateC triangle =
    { triangle | c = Position triangle.a.x triangle.b.y }


calcTrig : Model -> Model
calcTrig model =
    { model
        | cosA = calcCos model.angleA
        , cosB = calcCos model.angleB
        , sinA = calcSin model.angleA
        , sinB = calcSin model.angleB
    }


calcSin : Float -> Float
calcSin angle =
    angle
        |> degreesToRadians
        |> sin


calcCos : Float -> Float
calcCos angle =
    angle
        |> degreesToRadians
        |> cos


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


degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * pi / 180


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
