module MainTest exposing (..)

import Array
import Expect
import Game
import Goban
import Goban.Types as Goban
import Main exposing (gameHistoryContent)
import Test exposing (..)


tests : Test
tests =
    describe "gameHistoryContent"
        [ test "empty game" <|
            \_ ->
                Expect.equal
                    "GAME:\n-----"
                    (gameHistoryContent (Goban.empty 19))
        , test "game with two moves" <|
            \_ ->
                let
                    goban =
                        { size = 19
                        , moves =
                            Array.fromList
                                [ { color = Goban.Black, coords = ( 3, 4 ) }
                                , { color = Goban.White, coords = ( 10, 2 ) }
                                ]
                        }
                in
                Expect.equal
                    "GAME:\n-----\n01;B[ed]\n02;W[ck]"
                    (gameHistoryContent goban)
        ]
