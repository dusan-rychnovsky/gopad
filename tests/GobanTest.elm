module GobanTest exposing (all)

import Expect
import Goban exposing (..)
import Set exposing (Set)
import Test exposing (..)


all : Test
all =
    describe "Goban"
        [ describe "posToCoords"
            (let
                goban =
                    Goban 19 []

                imgSize =
                    900

                scale v =
                    round (v * imgSize / 2000)
             in
             [ test "center of board (should be (9,9))" <|
                \_ ->
                    Expect.equal (posToCoords goban (scale 1000) (scale 1000) imgSize) ( 9, 9 )
             , test "top-left corner (should be (0,0))" <|
                \_ ->
                    Expect.equal (posToCoords goban (scale 60) (scale 60) imgSize) ( 0, 0 )
             , test "bottom-right corner (should be (18,18))" <|
                \_ ->
                    let
                        px =
                            scale (2000 - 60)
                    in
                    Expect.equal (posToCoords goban px px imgSize) ( 18, 18 )
             , test "outside top-left clamps to (0,0)" <|
                \_ ->
                    Expect.equal (posToCoords goban 0 0 imgSize) ( 0, 0 )
             , test "outside bottom-right clamps to (18,18)" <|
                \_ ->
                    Expect.equal (posToCoords goban (scale 2000) (scale 2000) imgSize) ( 18, 18 )
             ]
            )
        , describe "currentPlayer"
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
