module Goban.Types exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Goban =
    { size : Int
    , moves : Array Move
    }


type alias Move =
    { color : Color
    , coords : Coords
    }


type Color
    = White
    | Black


type alias Row =
    Int


type alias Col =
    Int


type alias Coords =
    ( Row, Col )


type alias Situation =
    { gobanSize : Int
    , stones : Dict Coords Color
    }


type alias Group =
    Set Coords
