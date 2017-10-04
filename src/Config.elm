module Config exposing (config)

import Types exposing (Vector)


type alias Config =
    { offset : Vector }


config : Config
config =
    { offset = Vector 100 100 }
