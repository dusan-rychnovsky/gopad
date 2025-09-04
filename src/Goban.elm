module Goban exposing (Color(..), Coords, Goban, Move, currentPlayer)


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


type alias Coords =
    { row : Int
    , col : Int
    }


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
