module GobanTest exposing (all)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect
import Goban exposing (..)
import Goban.Types exposing (..)
import Set exposing (Set)
import Test exposing (..)


emptyGoban : Goban
emptyGoban =
    Goban.empty 19


imgSize : Int
imgSize =
    695


all : Test
all =
    describe "Goban"
        [ describe "coordsToPos"
            [ test "center of board" <|
                \_ ->
                    Expect.equal (coordsToPos ( 9, 9 ) imgSize) ( 354, 354 )
            , test "top-left corner" <|
                \_ ->
                    Expect.equal (coordsToPos ( 0, 0 ) imgSize) ( 30, 30 )
            , test "bottom-right corner" <|
                \_ ->
                    Expect.equal (coordsToPos ( 18, 18 ) imgSize) ( 678, 678 )
            ]
        , describe "posToCoords"
            [ test "center of board (should be (9,9))" <|
                \_ ->
                    Expect.equal (posToCoords 19 348 348 imgSize) ( 9, 9 )
            , test "top-left corner (should be (0,0))" <|
                \_ ->
                    Expect.equal (posToCoords 19 30 30 imgSize) ( 0, 0 )
            , test "bottom-right corner (should be (18,18))" <|
                \_ ->
                    Expect.equal (posToCoords 19 685 685 imgSize) ( 18, 18 )
            , test "outside top-left clamps to (0,0)" <|
                \_ ->
                    Expect.equal (posToCoords 19 0 0 imgSize) ( 0, 0 )
            , test "outside bottom-right clamps to (18,18)" <|
                \_ ->
                    Expect.equal (posToCoords 19 695 695 imgSize) ( 18, 18 )
            ]
        , describe "placeStone"
            [ test "places black stone on empty board" <|
                \_ ->
                    let
                        coords =
                            ( 3, 3 )

                        newGoban =
                            placeStone emptyGoban coords
                    in
                    Expect.equal (Array.toList newGoban.moves) [ { color = Black, coords = ( 3, 3 ) } ]
            , test "places white stone after black" <|
                \_ ->
                    let
                        goban =
                            Goban 19 (Array.fromList [ { color = Black, coords = ( 3, 3 ) } ])

                        coords =
                            ( 4, 4 )

                        newGoban =
                            placeStone goban coords
                    in
                    Expect.equal (Array.toList newGoban.moves) [ { color = Black, coords = ( 3, 3 ) }, { color = White, coords = ( 4, 4 ) } ]
            , test "places black stone after black and white" <|
                \_ ->
                    let
                        goban =
                            Goban 19
                                (Array.fromList
                                    [ { color = Black, coords = ( 3, 3 ) }
                                    , { color = White, coords = ( 4, 4 ) }
                                    ]
                                )

                        coords =
                            ( 5, 5 )

                        newGoban =
                            placeStone goban coords
                    in
                    Expect.equal (Array.toList newGoban.moves) [ { color = Black, coords = ( 3, 3 ) }, { color = White, coords = ( 4, 4 ) }, { color = Black, coords = ( 5, 5 ) } ]
            ]
        , describe "undoMove"
            [ test "on empty goban does nothing" <|
                \_ ->
                    Expect.equal (undoMove emptyGoban) emptyGoban
            , test "removes the last move" <|
                \_ ->
                    let
                        move1 =
                            { color = Black, coords = ( 3, 4 ) }

                        move2 =
                            { color = White, coords = ( 5, 3 ) }

                        goban =
                            Goban 19 (Array.fromList [ move1, move2 ])
                    in
                    Expect.equal (undoMove goban) (Goban 19 (Array.fromList [ move1 ]))
            , test "on goban with one move results in empty board" <|
                \_ ->
                    let
                        goban =
                            Goban 19 (Array.fromList [ { color = Black, coords = ( 3, 3 ) } ])
                    in
                    Expect.equal (undoMove goban) emptyGoban
            ]
        , describe "placeHandicapStone"
            [ test "two handicap and a regular move" <|
                \_ ->
                    let
                        goban0 =
                            empty 19

                        goban1 =
                            placeHandicapStone goban0 ( 3, 3 )

                        goban2 =
                            placeHandicapStone goban1 ( 3, 15 )

                        goban3 =
                            placeStone goban2 ( 15, 3 )
                    in
                    Expect.equal
                        [ { color = Black, coords = ( 3, 3 ) }
                        , { color = Black, coords = ( 3, 15 ) }
                        , { color = White, coords = ( 15, 3 ) }
                        ]
                        (Array.toList goban3.moves)
            ]
        ]
