module GobanTest exposing (all)

import Expect
import Goban exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Goban"
        [ describe "currentPlayer"
            [ test "is black when board is empty" <|
                \_ ->
                    Expect.equal (currentPlayer (Goban 19 [])) Black
            , test "is white after black plays" <|
                \_ ->
                    let
                        goban =
                            Goban 19 [ { color = Black, coords = { row = 3, col = 3 } } ]
                    in
                    Expect.equal (currentPlayer goban) White
            , test "is black after white plays" <|
                \_ ->
                    let
                        goban =
                            Goban 19
                                [ { color = Black, coords = { row = 3, col = 3 } }
                                , { color = White, coords = { row = 16, col = 16 } }
                                ]
                    in
                    Expect.equal (currentPlayer goban) Black
            ]
        ]
