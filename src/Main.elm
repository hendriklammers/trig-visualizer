module Main exposing (..)

import Html exposing (programWithFlags)
import Task
import View
import Model exposing (Model)
import Messages exposing (..)
import Mouse
import Types exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.ups DragEnd
                ]


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        halfWidth =
            round (toFloat flags.width / 2)

        halfHeight =
            round (toFloat flags.height / 2)

        initialModel =
            { triangle =
                { a = Position 0 0
                , b = Position halfWidth halfHeight
                , c = Position 0 halfHeight
                }
            , lengthAB = 0
            , lengthAC = 0
            , lengthBC = 0
            , angleA = 0
            , angleB = 0
            , cosA = 0
            , sinA = 0
            , cosB = 0
            , sinB = 0
            , drag = Nothing
            , unit = Pixel
            , width = flags.width
            , height = flags.height
            }

        commands =
            [ Task.perform UpdateTriangle (Task.succeed initialModel.triangle)
            ]
    in
        ( initialModel
        , Cmd.batch commands
        )


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = Model.update
        , view = View.view
        }
