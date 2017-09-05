module Main exposing (..)

import Html exposing (Html, text, program)
import Window exposing (Size)
import Task


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
    text <| toString model.windowSize



-- Main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
