module Main exposing (..)

import Html exposing (Html, text, program)


-- Model


type alias Model =
    String


initialModel : Model
initialModel =
    "Trig Visualizer"



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    text model



-- Main


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
