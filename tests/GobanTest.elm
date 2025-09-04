module GobanTest exposing (all)

import Expect
import Goban exposing (..)
import Set exposing (Set)
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
                            Goban 19 [ { color = Black, coords = ( 3, 3 ) } ]
                    in
                    Expect.equal (currentPlayer goban) White
            , test "is black after white plays" <|
                \_ ->
                    let
                        goban =
                            Goban 19
                                [ { color = Black, coords = ( 3, 3 ) }
                                , { color = White, coords = ( 16, 16 ) }
                                ]
                    in
                    Expect.equal (currentPlayer goban) Black
            ]
        , describe "getNeighbours"
            [ test "for a stone in the center of the board returns 4 neighbours" <|
                \_ ->
                    let
                        goban =
                            Goban 19 []

                        coords =
                            ( 9, 9 )

                        expected =
                            Set.fromList
                                [ ( 8, 9 )
                                , ( 10, 9 )
                                , ( 9, 8 )
                                , ( 9, 10 )
                                ]
                    in
                    Expect.equal (getNeighbours goban coords) expected
            , test "for a stone in the corner of the board returns 2 neighbours" <|
                \_ ->
                    let
                        goban =
                            Goban 19 []

                        coords =
                            ( 0, 0 )

                        expected =
                            Set.fromList
                                [ ( 1, 0 )
                                , ( 0, 1 )
                                ]
                    in
                    Expect.equal (getNeighbours goban coords) expected
            , test "for a stone on the edge of the board returns 3 neighbours" <|
                \_ ->
                    let
                        goban =
                            Goban 19 []

                        coords =
                            ( 0, 1 )

                        expected =
                            Set.fromList
                                [ ( 0, 0 )
                                , ( 0, 2 )
                                , ( 1, 1 )
                                ]
                    in
                    Expect.equal (getNeighbours goban coords) expected
            ]
        ]
