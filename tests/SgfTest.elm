module SgfTest exposing (all)

import Expect
import Game exposing (..)
import Goban exposing (..)
import Sgf exposing (toSgf)
import Test exposing (..)


dummyModel : Game
dummyModel =
    { name = "Test Game"
    , whitePlayer = "Alice"
    , blackPlayer = "Bob"
    , date = "2025-09-08 17:05"
    , goban = { size = 19, moves = [] }
    }


all : Test
all =
    describe "Sgf.toSgf"
        [ test "returns empty string for empty model" <|
            \_ ->
                Expect.equal "" (toSgf dummyModel)
        ]
