module Messages exposing (..)

import Types exposing (Triangle, Position, Unit)


type Msg
    = UpdateTriangle Triangle
    | ChangeUnit Unit
    | DragStart Position
    | DragAt Position
    | DragEnd Position
