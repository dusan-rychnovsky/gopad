module Goban exposing (coordsToPos, currentSituation, empty, isEmpty, placeHandicapStone, placeStone, posToCoords, undoMove)

import Array exposing (Array)
import Dict exposing (Dict)
import Goban.Internal exposing (..)
import Goban.Types exposing (..)
import Set exposing (Set)


gobanImg =
    { sizePx = 2000.0
    , paddingPx = 60.0
    , squarePx = 99.0
    , linePx = 5.0
    }


posToCoords : Int -> Int -> Int -> Int -> Coords
posToCoords gobanSize posX posY imgSize =
    let
        scale =
            gobanImg.sizePx / toFloat imgSize

        x =
            toFloat posX * scale

        y =
            toFloat posY * scale

        step =
            gobanImg.squarePx + gobanImg.linePx

        clamp v =
            if v < 0 then
                0

            else if v > gobanSize - 1 then
                gobanSize - 1

            else
                v

        col =
            clamp (round ((x - gobanImg.paddingPx) / step))

        row =
            clamp (round ((y - gobanImg.paddingPx) / step))
    in
    ( row, col )


coordsToPos : Coords -> Int -> ( Int, Int )
coordsToPos ( row, col ) imgSize =
    let
        scale =
            toFloat imgSize / gobanImg.sizePx

        step =
            gobanImg.squarePx + gobanImg.linePx

        x =
            (gobanImg.paddingPx + toFloat col * step) * scale

        y =
            (gobanImg.paddingPx + toFloat row * step) * scale
    in
    ( round x, round y )


empty : Int -> Goban
empty size =
    { size = size
    , moves = Array.empty
    }


isEmpty : Goban -> Bool
isEmpty goban =
    Array.isEmpty goban.moves


placeStone : Goban -> Coords -> Goban
placeStone goban coords =
    let
        newMove =
            { color = currentPlayer goban
            , coords = coords
            }
    in
    { goban | moves = Array.push newMove goban.moves }


placeHandicapStone : Goban -> Coords -> Goban
placeHandicapStone goban coords =
    let
        newMove =
            { color = Black
            , coords = coords
            }
    in
    { goban | moves = Array.push newMove goban.moves }


undoMove : Goban -> Goban
undoMove goban =
    { goban | moves = Array.slice 0 (Array.length goban.moves - 1) goban.moves }


currentSituation : Goban -> Situation
currentSituation goban =
    Array.foldl applyMove { gobanSize = goban.size, stones = Dict.empty } goban.moves
