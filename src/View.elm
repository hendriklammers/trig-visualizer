module View exposing (..)

import Html exposing (Html, text, program, div, span)
import Html.Attributes as H
import Svg exposing (Svg, g, svg, rect, polygon, defs, text_)
import Svg.Attributes as S
import Model exposing (Model, Triangle)
import Vector exposing (Vector, distance)
import Config exposing (config)
import Messages exposing (..)


view : Model -> Html Msg
view model =
    div
        [ H.class "container" ]
        [ viewSvg model
        , viewValues model
        ]


formatFloat : Float -> String
formatFloat n =
    (n * 100 |> round |> toFloat) / 100 |> toString


viewLength : String -> ( Vector, Vector ) -> Html Msg
viewLength label ( p1, p2 ) =
    let
        str =
            distance p1 p2
                |> formatFloat
    in
        div
            [ H.class "textfield" ]
            [ span [ H.class "textfield__label" ] [ text label ]
            , text str
            ]


viewValues : Model -> Html Msg
viewValues { triangle } =
    div [ H.class "textfields" ]
        [ viewLength "length AB" ( triangle.a, triangle.b )
        , viewLength "length AC" ( triangle.a, triangle.c )
        , viewLength "length BC" ( triangle.b, triangle.c )
        ]


translateVector : Vector -> String
translateVector v =
    "translate(" ++ toString v.x ++ " " ++ toString v.y ++ ")"


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
            [ defs
                []
                []
            , rect
                [ S.width w, S.height h, S.fill "#eee" ]
                []
            , g
                [ S.transform <| translateVector config.offset ]
                [ viewTriangle model.triangle
                , viewHandle model.triangle.a
                , viewHandle model.triangle.b
                ]
            , g
                [ S.transform <| translateVector config.offset ]
                [ viewLabel "A" (Vector (model.triangle.a.x + 15) -10)
                , viewLabel "B" (Vector -10 (model.triangle.b.y + 15))
                , viewLabel "C" (Vector -10 -10)
                ]
            ]


viewLabel : String -> Vector -> Svg Msg
viewLabel label { x, y } =
    text_
        [ S.textAnchor "middle"
        , S.x <| toString x
        , S.y <| toString y
        ]
        [ Svg.text label ]


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
