module Main exposing (..)

import Html exposing (Html, text, program)
import Svg exposing (Svg, svg, rect, polygon)
import Svg.Attributes as S
import Task
import Window exposing (Size)


-- Model


type alias Model =
    { windowSize : Size
    , points : List Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


initialModel : Model
initialModel =
    { windowSize = Size 0 0
    , points =
        [ Point 200 200
        , Point 600 400
        , Point 200 400
        ]
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
            , svgTriangle model.points
            ]


svgTriangle : List Point -> Svg Msg
svgTriangle points =
    polygon
        [ S.points <| pointString points
        , S.fill "#000"
        ]
        []


pointString : List Point -> String
pointString points =
    points
        |> List.map (\p -> toString p.x ++ "," ++ toString p.y)
        |> String.join " "



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
