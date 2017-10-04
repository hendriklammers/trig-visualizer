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
    let
        initial =
            Model.initial
    in
        program
            { init =
                ( initial
                , Cmd.batch
                    [ Task.perform WindowResize Window.size
                    , Task.perform UpdateTriangle
                        (Task.succeed initial.triangle)
                    ]
                )
            , subscriptions = subscriptions
            , update = Model.update
            , view = View.view
            }
