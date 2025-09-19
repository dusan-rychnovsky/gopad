module GameTest exposing (..)

import Expect
import Game exposing (Game, name)
import Goban
import Test exposing (..)


game : String -> String -> String -> String -> Game
game date black white location =
    { date = date
    , blackPlayer = black
    , whitePlayer = white
    , location = location
    , placingHandicapMode = False
    , goban = Goban.empty 19
    }


all : Test
all =
    describe "Game"
        [ describe "name"
            [ test "all fields empty" <|
                \_ -> Expect.equal "" (name (game "" "" "" ""))
            , test "only date" <|
                \_ -> Expect.equal "2025-09-17" (name (game "2025-09-17" "" "" ""))
            , test "date and black" <|
                \_ -> Expect.equal "2025-09-17 Lee" (name (game "2025-09-17" "Lee" "" ""))
            , test "date, black, white" <|
                \_ -> Expect.equal "2025-09-17 Lee Park" (name (game "2025-09-17" "Lee" "Park" ""))
            , test "all fields" <|
                \_ -> Expect.equal "2025-09-17 Lee Park Tokyo" (name (game "2025-09-17" "Lee" "Park" "Tokyo"))
            , test "fields with extra spaces" <|
                \_ -> Expect.equal "2025-09-17 Lee Park Tokyo" (name (game " 2025-09-17 " " Lee " " Park " " Tokyo "))
            , test "some fields empty, some with spaces" <|
                \_ -> Expect.equal "Lee Tokyo" (name (game "" " Lee " "" " Tokyo "))
            ]
        ]
