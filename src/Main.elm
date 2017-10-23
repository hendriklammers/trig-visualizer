module Main exposing (..)

import Html exposing (program)
import Task
import View
import Model exposing (Model)
import Messages exposing (..)
import Mouse


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.ups DragEnd
                ]


main : Program Never Model Msg
main =
    let
        initial =
            Model.initial

        commands =
            [ Task.perform UpdateTriangle (Task.succeed initial.triangle)
            ]
    in
        program
            { init =
                ( initial
                , Cmd.batch commands
                )
            , subscriptions = subscriptions
            , update = Model.update
            , view = View.view
            }
