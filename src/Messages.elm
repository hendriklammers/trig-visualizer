module Messages exposing (..)

import Types exposing (Triangle, Position)


type Msg
    = UpdateTriangle Triangle
    | ToggleNormalize
    | DragStart Position
    | DragAt Position
    | DragEnd Position
