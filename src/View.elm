module View exposing (..)

import Html exposing (Html, text, program, div, span, input, label)
import Html.Attributes as H
import Html.Events exposing (onClick)
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
import Types exposing (Vector, Triangle, Length)
import Config exposing (config)
import Messages exposing (..)


view : Model -> Html Msg
view model =
    div
        [ H.class "container" ]
        [ viewModel model
        , viewToggles model
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
            , viewAngles model
            ]


viewAngles : Model -> Svg Msg
viewAngles model =
    g
        [ S.transform <| translateVector config.offset ]
        [ text_
            [ S.textAnchor "start"
            , S.x <| toString <| model.triangle.a.x
            , S.y <| toString <| model.triangle.a.y - 25
            , S.fontSize "16"
            ]
            [ Svg.text <| formatFloat model.angleA ++ "°" ]
        , text_
            [ S.textAnchor "start"
            , S.x <| toString <| model.triangle.b.x + 25
            , S.y <| toString <| model.triangle.b.y
            , S.fontSize "16"
            ]
            [ Svg.text <| formatFloat model.angleB ++ "°" ]
        ]


viewLengths : Model -> Svg Msg
viewLengths model =
    let
        ls =
            [ Length
                { x = -10
                , y = round <| model.lengthAC / 2
                }
                -90
                model.lengthAC
            , Length
                { x = round <| model.lengthBC / 2
                , y = round model.lengthAC + 20
                }
                0
                model.lengthBC
            , Length
                { x = round <| model.lengthBC / 2 + 5
                , y = round <| model.lengthAC / 2 - 5
                }
                model.angleB
                model.lengthAB
            ]

        normalized =
            if model.normalize then
                List.map
                    (\ln -> { ln | value = ln.value / model.lengthAB })
                    ls
            else
                ls
    in
        g
            [ S.transform <| translateVector config.offset ]
            (List.map viewLength normalized)


viewLength : Length -> Svg Msg
viewLength { position, rotation, value } =
    let
        r =
            "rotate("
                ++ toString rotation
                ++ " "
                ++ toString position.x
                ++ ","
                ++ toString position.y
                ++ ")"
    in
        text_
            [ S.textAnchor "middle"
            , S.x <| toString position.x
            , S.y <| toString position.y
            , S.transform r
            ]
            [ Svg.text <| formatFloat value ]


viewLabels : Model -> Svg Msg
viewLabels model =
    g
        [ S.transform <| translateVector config.offset ]
        [ viewLabel "A" (Vector -10 -10)
        , viewLabel "B"
            (Vector
                (model.triangle.b.x + 15)
                (model.triangle.b.y + 20)
            )
        , viewLabel "C" (Vector -10 (model.triangle.c.y + 20))
        ]


viewLabel : String -> Vector -> Svg Msg
viewLabel label { x, y } =
    text_
        [ S.textAnchor "middle"
        , S.x <| toString x
        , S.y <| toString y
        , S.fontSize "18"
        ]
        [ Svg.text label ]


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


cornerAttributes : List (Svg.Attribute Msg)
cornerAttributes =
    [ S.stroke "#c00"
    , S.strokeWidth "1"
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


viewToggles : Model -> Html Msg
viewToggles model =
    label
        []
        [ input
            [ H.type_ "checkbox"
            , H.style [ ( "margin-right", "5px" ) ]
            , onClick ToggleNormalize
            ]
            []
        , text "Normalize sides"
        ]


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
