module SgfTest exposing (all)

import Array exposing (Array)
import Expect
import Game exposing (..)
import Goban
import Goban.Types as Goban
import Sgf exposing (fileName, toSgf)
import Test exposing (..)


game : Game
game =
    { date = "2025-09-08"
    , blackPlayer = "Bob"
    , whitePlayer = "Alice"
    , location = "Wonderland"
    , goban = Goban.empty 19
    }


initGame : String -> String -> String -> String -> Game
initGame date black white location =
    { date = date
    , blackPlayer = black
    , whitePlayer = white
    , location = location
    , goban = Goban.empty 19
    }


all : Test
all =
    describe "Sgf"
        [ describe "toSgf"
            [ test "a game with no moves" <|
                \_ ->
                    Expect.equal
                        ( "2025-09-08-Bob-Alice-Wonderland.sgf"
                        , "(;FF[4]GM[1]AP[gopad:0.1]GN[2025-09-08 Bob Alice Wonderland]DT[2025-09-08]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n\n)"
                        )
                        (toSgf game)
            , test "a game with one black move" <|
                \_ ->
                    Expect.equal
                        ( "2025-09-08-Bob-Alice-Wonderland.sgf"
                        , "(;FF[4]GM[1]AP[gopad:0.1]GN[2025-09-08 Bob Alice Wonderland]DT[2025-09-08]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n;B[dq]\n)"
                        )
                        (toSgf { game | goban = { size = 19, moves = Array.fromList [ { color = Goban.Black, coords = ( 16, 3 ) } ] } })
            , test "a game with one black and one white move" <|
                \_ ->
                    Expect.equal
                        ( "2025-09-08-Bob-Alice-Wonderland.sgf"
                        , "(;FF[4]GM[1]AP[gopad:0.1]GN[2025-09-08 Bob Alice Wonderland]DT[2025-09-08]PB[Bob]PW[Alice]SZ[19]KM[6.5]\n;B[dq]\n;W[dd]\n)"
                        )
                        (toSgf { game | goban = { size = 19, moves = Array.fromList [ { color = Goban.Black, coords = ( 16, 3 ) }, { color = Goban.White, coords = ( 3, 3 ) } ] } })
            ]
        , describe "fileName"
            [ test "all fields empty" <|
                \_ -> Expect.equal "game.sgf" (fileName (initGame "" "" "" ""))
            , test "only white space" <|
                \_ -> Expect.equal "game.sgf" (fileName (initGame " " " " "   " " "))
            , test "only date" <|
                \_ -> Expect.equal "2025-09-17.sgf" (fileName (initGame "2025-09-17" "" "" ""))
            , test "date and black" <|
                \_ -> Expect.equal "2025-09-17-Lee.sgf" (fileName (initGame "2025-09-17" "Lee" "" ""))
            , test "date, black, white" <|
                \_ -> Expect.equal "2025-09-17-Lee-Park.sgf" (fileName (initGame "2025-09-17" "Lee" "Park" ""))
            , test "all fields" <|
                \_ -> Expect.equal "2025-09-17-Lee-Park-Tokyo.sgf" (fileName (initGame "2025-09-17" "Lee" "Park" "Tokyo"))
            , test "fields with extra spaces" <|
                \_ -> Expect.equal "2025-09-17-Lee-Park-Tokyo.sgf" (fileName (initGame " 2025-09-17 " " Lee " " Park " " Tokyo "))
            , test "some fields empty, some with spaces" <|
                \_ -> Expect.equal "Lee-Tokyo.sgf" (fileName (initGame "" " Lee " "" " Tokyo "))
            ]
        ]
