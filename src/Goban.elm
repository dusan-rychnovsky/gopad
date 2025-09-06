module Goban exposing (Color(..), Coords, Goban, Move, coordsToPos, currentPlayer, currentPosition, getAdjacentPoints, placeStone, posToCoords)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Goban =
    { size : Int
    , moves : List Move
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


type alias Position =
    Dict Coords Color


gobanImg =
    { sizePx = 2000.0
    , paddingPx = 60.0
    , squarePx = 99.0
    , linePx = 5.0
    }


posToCoords : Goban -> Int -> Int -> Int -> Coords
posToCoords goban posX posY imgSize =
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

            else if v > goban.size - 1 then
                goban.size - 1

            else
                v

        col =
            clamp (round ((x - gobanImg.paddingPx) / step))

        row =
            clamp (round ((y - gobanImg.paddingPx) / step))
    in
    ( row, col )


coordsToPos : Goban -> Coords -> Int -> ( Int, Int )
coordsToPos goban ( row, col ) imgSize =
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


placeStone : Goban -> Coords -> Goban
placeStone goban coords =
    let
        newMove =
            { color = currentPlayer goban
            , coords = coords
            }
    in
    { goban | moves = goban.moves ++ [ newMove ] }


currentPlayer : Goban -> Color
currentPlayer goban =
    case List.head (List.reverse goban.moves) of
        Just lastMove ->
            case lastMove.color of
                White ->
                    Black

                Black ->
                    White

        Nothing ->
            Black


getAdjacentPoints : Int -> Coords -> Set Coords
getAdjacentPoints gobanSize ( row, col ) =
    let
        candidates =
            [ ( row - 1, col )
            , ( row + 1, col )
            , ( row, col - 1 )
            , ( row, col + 1 )
            ]
    in
    candidates
        |> List.filter (\( r, c ) -> r >= 0 && c >= 0 && r < gobanSize && c < gobanSize)
        |> Set.fromList


currentPosition : Goban -> Position
currentPosition goban =
    List.foldl applyMove Dict.empty goban.moves


applyMove : Move -> Position -> Position
applyMove move position =
    Dict.insert move.coords move.color position
