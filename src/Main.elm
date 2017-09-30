module Main exposing (..)

import Html exposing (program)
import Task
import View
import Model exposing (Model)
import Messages exposing (..)
import Window


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowResize


main : Program Never Model Msg
main =
    program
        { init = ( Model.initial, Task.perform WindowResize Window.size )
        , subscriptions = subscriptions
        , update = Model.update
        , view = View.view
        }
