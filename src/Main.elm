module Main exposing (..)

import Html exposing (Html, text, program)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (width, height, viewBox, fill)
import Task
import Window exposing (Size)


-- Model


type alias Model =
    { windowSize : Size }


initialModel : Model
initialModel =
    { windowSize = Size 0 0 }


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
            [ width w, height h, viewBox ("0 0 " ++ w ++ " " ++ h) ]
            [ rect
                [ width w, height h, fill "#eee" ]
                []
            ]



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
