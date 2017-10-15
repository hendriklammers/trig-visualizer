module Messages exposing (..)

import Types exposing (Triangle)
import Mouse exposing (Position)


type Msg
    = UpdateTriangle Triangle
    | ToggleNormalize
    | DragStart Position
    | DragAt Position
    | DragEnd Position
