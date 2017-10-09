module Messages exposing (..)

import Window exposing (Size)
import Types exposing (Triangle)


type Msg
    = WindowResize Size
    | UpdateTriangle Triangle
    | ToggleNormalize
