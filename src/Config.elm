module Config exposing (config)

import Vector exposing (Vector)


type alias Config =
    { offset : Vector }


config : Config
config =
    { offset = Vector 100 100 }
