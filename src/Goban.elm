module Goban exposing (Goban, Move, Color(..), Coords)

type alias Goban =
    { size: Int
    , moves : List Move
    }

type alias Move = 
    { color: Color
    , coords: Coords
    }

type Color
    = White
    | Black

type alias Coords =
    { row : Int
    , col : Int
    }
