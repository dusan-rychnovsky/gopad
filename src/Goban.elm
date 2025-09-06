module Goban exposing (Color(..), Coords, Goban, Move, currentPlayer, getNeighbours, posToCoords)

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


gobanImg =
    { sizePx = 2000.0
    , paddingPx = 60.0
    , squarePx = 99.0
    , linePx = 5.0
    }


posToCoords : Goban -> Int -> Int -> Int -> ( Int, Int )
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


getNeighbours : Goban -> Coords -> Set Coords
getNeighbours goban ( row, col ) =
    let
        candidates =
            [ ( row - 1, col )
            , ( row + 1, col )
            , ( row, col - 1 )
            , ( row, col + 1 )
            ]
    in
    candidates
        |> List.filter (\( r, c ) -> r >= 0 && c >= 0 && r < goban.size && c < goban.size)
        |> Set.fromList
