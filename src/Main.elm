module Main exposing (..)

import Html exposing (Html, text, program)
import Svg exposing (Svg, g, svg, rect, polygon)
import Svg.Attributes as S
import Task
import Window exposing (Size)


-- Model


type alias Model =
    { windowSize : Size
    , triangle : Triangle
    }


type alias Triangle =
    { a : Vector
    , b : Vector
    , c : Vector
    }


type alias Vector =
    { x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { windowSize = Size 0 0
    , triangle =
        { a = Vector 400 0
        , b = Vector 0 300
        , c = Vector 0 0
        }
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform WindowResize Window.size )



-- Update


type Msg
    = WindowResize Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowResize size ->
            ( { model | windowSize = size }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize



-- View


view : Model -> Html Msg
view model =
    viewSvg model


viewSvg : Model -> Html Msg
viewSvg model =
    let
        w =
            toString model.windowSize.width

        h =
            toString model.windowSize.height
    in
        svg
            [ S.width w, S.height h, S.viewBox ("0 0 " ++ w ++ " " ++ h) ]
            [ rect
                [ S.width w, S.height h, S.fill "#eee" ]
                []
            , g [ S.transform "translate(100 100)" ]
                [ viewTriangle model.triangle
                , viewHandle model.triangle.a
                , viewHandle model.triangle.b
                ]
            ]


viewTriangle : Triangle -> Svg Msg
viewTriangle triangle =
    polygon
        [ S.points <| pointString triangle
        , S.fill "#fff"
        , S.stroke "#000"
        , S.strokeWidth "2"
        ]
        []


viewHandle : Vector -> Svg Msg
viewHandle { x, y } =
    rect
        [ S.x <| toString (x - 5)
        , S.y <| toString (y - 5)
        , S.width "10"
        , S.height "10"
        , S.fill "#e81778"
        ]
        []


pointString : Triangle -> String
pointString { a, b, c } =
    let
        str { x, y } =
            toString x ++ "," ++ toString y
    in
        str a ++ " " ++ str b ++ " " ++ str c



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
