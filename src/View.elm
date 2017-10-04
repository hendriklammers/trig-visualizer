module View exposing (..)

import Html exposing (Html, text, program, div, span)
import Html.Attributes as H
import Svg
    exposing
        ( Svg
        , g
        , svg
        , rect
        , polygon
        , defs
        , text_
        , clipPath
        , circle
        )
import Svg.Attributes as S
import Model exposing (Model)
import Types exposing (Vector, Triangle)
import Config exposing (config)
import Messages exposing (..)


view : Model -> Html Msg
view model =
    div
        [ H.class "container" ]
        [ viewModel model
        , viewSvg model
        ]


viewModel : Model -> Html Msg
viewModel model =
    div
        []
        [ text <| toString model
        ]


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
                [ clipPath
                    [ S.id "mask" ]
                    [ polygon
                        [ S.points <| pointString model.triangle ]
                        []
                    ]
                ]
            , rect
                [ S.width w, S.height h, S.fill "#eee" ]
                []
            , viewTriangle model.triangle
            , viewLabels model
            , viewLengths model
            ]


viewLengths : Model -> Svg Msg
viewLengths model =
    let
        posAC =
            { x = round <| model.lengthAC / 2
            , y = -10
            }

        posBC =
            { x = -10
            , y = round <| model.lengthBC / 2
            }

        posAB =
            { x = round <| model.lengthAC / 2 + 15
            , y = round <| model.lengthBC / 2 + 15
            }
    in
        g
            [ S.transform <| translateVector config.offset ]
            [ viewLength posAC 0 model.lengthAC
            , viewLength posBC -90 model.lengthBC
            , viewLength posAB -45 model.lengthAB
            ]


viewLength : Vector -> Int -> Float -> Svg Msg
viewLength { x, y } rotation length =
    let
        r =
            "rotate("
                ++ toString rotation
                ++ " "
                ++ toString x
                ++ ","
                ++ toString y
                ++ ")"
    in
        text_
            [ S.textAnchor "middle"
            , S.x <| toString x
            , S.y <| toString y
            , S.transform r
            ]
            [ Svg.text <| formatFloat length ]


viewTriangle : Triangle -> Html Msg
viewTriangle triangle =
    let
        poly =
            polygon
                [ S.points <| pointString triangle
                , S.fill "#fff"
                , S.stroke "#000"
                , S.strokeWidth "2"
                ]
                []
    in
        g
            [ S.transform <| translateVector config.offset ]
            [ poly
            , viewCornerCircle triangle.a
            , viewCornerCircle triangle.b
            , viewCornerRect triangle.c
            ]


viewLabels : Model -> Svg Msg
viewLabels model =
    g
        [ S.transform <| translateVector config.offset ]
        [ viewLabel "A" (Vector (model.triangle.a.x + 15) -10)
        , viewLabel "B" (Vector -10 (model.triangle.b.y + 20))
        , viewLabel "C" (Vector -10 -10)
        ]


viewLabel : String -> Vector -> Svg Msg
viewLabel label { x, y } =
    text_
        [ S.textAnchor "middle"
        , S.x <| toString x
        , S.y <| toString y
        ]
        [ Svg.text label ]


cornerAttributes : List (Svg.Attribute Msg)
cornerAttributes =
    [ S.stroke "#c00"
    , S.strokeWidth "2"
    , S.fill "none"
    , S.opacity "0.75"
    , S.clipPath "url(#mask)"
    ]


viewCornerCircle : Vector -> Svg Msg
viewCornerCircle { x, y } =
    circle
        (cornerAttributes
            ++ [ S.cx <| toString x
               , S.cy <| toString y
               , S.r "25"
               ]
        )
        []


viewCornerRect : Vector -> Html Msg
viewCornerRect { x, y } =
    let
        size =
            40
    in
        rect
            (cornerAttributes
                ++ [ S.x <| toString <| x - floor (size / 2)
                   , S.y <| toString <| y - floor (size / 2)
                   , S.width <| toString size
                   , S.height <| toString size
                   ]
            )
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


formatFloat : Float -> String
formatFloat n =
    (n * 100 |> round |> toFloat) / 100 |> toString


translateVector : Vector -> String
translateVector v =
    "translate(" ++ toString v.x ++ " " ++ toString v.y ++ ")"
