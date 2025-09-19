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


all : Test
all =
    describe "Goban"
        [ describe "coordsToPos"
            (let
                imgSize =
                    900

                scale v =
                    round (v * imgSize / 2000)
             in
             [ test "center of board" <|
                \_ ->
                    Expect.equal (coordsToPos ( 9, 9 ) imgSize)
                        ( scale 996, scale 996 )
             , test "top-left corner" <|
                \_ ->
                    Expect.equal (coordsToPos ( 0, 0 ) imgSize)
                        ( scale 60, scale 60 )
             , test "bottom-right corner" <|
                \_ ->
                    Expect.equal (coordsToPos ( 18, 18 ) imgSize)
                        ( scale 1932, scale 1932 )
             ]
            )
        , describe "posToCoords"
            (let
                imgSize =
                    900

                scale v =
                    round (v * imgSize / 2000)
             in
             [ test "center of board (should be (9,9))" <|
                \_ ->
                    Expect.equal (posToCoords 19 (scale 1000) (scale 1000) imgSize) ( 9, 9 )
             , test "top-left corner (should be (0,0))" <|
                \_ ->
                    Expect.equal (posToCoords 19 (scale 60) (scale 60) imgSize) ( 0, 0 )
             , test "bottom-right corner (should be (18,18))" <|
                \_ ->
                    let
                        px =
                            scale (2000 - 60)
                    in
                    Expect.equal (posToCoords 19 px px imgSize) ( 18, 18 )
             , test "outside top-left clamps to (0,0)" <|
                \_ ->
                    Expect.equal (posToCoords 19 0 0 imgSize) ( 0, 0 )
             , test "outside bottom-right clamps to (18,18)" <|
                \_ ->
                    Expect.equal (posToCoords 19 (scale 2000) (scale 2000) imgSize) ( 18, 18 )
             ]
            )
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
