module Messages exposing (..)

import Types exposing (Triangle, Position, LengthUnit)


type Msg
    = UpdateTriangle Triangle
    | ChangeLengthUnit LengthUnit
    | DragStart Position
    | DragAt Position
    | DragEnd Position
    | KeyDown Int
    | KeyUp Int
