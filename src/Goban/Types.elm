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
    , captures : Dict String Int
    }


type alias Group =
    Set Coords


colorToString : Color -> String
colorToString color =
    case color of
        Black ->
            "B"

        White ->
            "W"


numCaptured : Situation -> Color -> Int
numCaptured situation color =
    Dict.get (colorToString color) situation.captures
        |> Maybe.withDefault 0


incCaptured : Situation -> Color -> Int -> Situation
incCaptured situation color num =
    let
        totalCaptured =
            num + numCaptured situation color
    in
    { situation | captures = Dict.insert (colorToString color) totalCaptured situation.captures }
