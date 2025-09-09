module SgfTest exposing (all)

import Expect
import Game exposing (..)
import Goban exposing (..)
import Sgf exposing (toSgf)
import Test exposing (..)


game : Game
game =
    { name = "Test Game"
    , whitePlayer = "Alice"
    , blackPlayer = "Bob"
    , date = "2025-09-08 17:05"
    , posix = Nothing
    , timeZone = Nothing
    , goban = { size = 19, moves = [] }
    }


all : Test
all =
    describe "toSgf"
        [ test "a game with no moves" <|
            \_ ->
                Expect.equal
                    ( "game.sgf"
                    , "(;FF[4]GM[1]AP[gopad:0.1]GN[Test Game]DT[2025-09-08 17:05]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n\n)"
                    )
                    (toSgf game)
        , test "a game with one black move" <|
            \_ ->
                Expect.equal
                    ( "game.sgf"
                    , "(;FF[4]GM[1]AP[gopad:0.1]GN[Test Game]DT[2025-09-08 17:05]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n;B[dq]\n)"
                    )
                    (toSgf { game | goban = { size = 19, moves = [ { color = Black, coords = ( 16, 3 ) } ] } })
        , test "a game with one black and one white move" <|
            \_ ->
                Expect.equal
                    ( "game.sgf"
                    , "(;FF[4]GM[1]AP[gopad:0.1]GN[Test Game]DT[2025-09-08 17:05]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n;B[dq]\n;W[dd]\n)"
                    )
                    (toSgf { game | goban = { size = 19, moves = [ { color = Black, coords = ( 16, 3 ) }, { color = White, coords = ( 3, 3 ) } ] } })
        ]
