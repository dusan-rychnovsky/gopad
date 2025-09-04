module Goban exposing (Color(..), Coords, Goban, Move, currentPlayer, getNeighbours)

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
