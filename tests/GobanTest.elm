module GobanTest exposing (all)

import Dict exposing (Dict)
import Expect
import Goban exposing (..)
import Set exposing (Set)
import Test exposing (..)


all : Test
all =
    describe "Goban"
        [ describe "coordsToPos"
            (let
                goban =
                    Goban 19 []

                imgSize =
                    900

                scale v =
                    round (v * imgSize / 2000)
             in
             [ test "center of board" <|
                \_ ->
                    Expect.equal (coordsToPos goban ( 9, 9 ) imgSize)
                        ( scale 996, scale 996 )
             , test "top-left corner" <|
                \_ ->
                    Expect.equal (coordsToPos goban ( 0, 0 ) imgSize)
                        ( scale 60, scale 60 )
             , test "bottom-right corner" <|
                \_ ->
                    Expect.equal (coordsToPos goban ( 18, 18 ) imgSize)
                        ( scale 1932, scale 1932 )
             ]
            )
        , describe "posToCoords"
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
        , describe "placeStone"
            [ test "places black stone on empty board" <|
                \_ ->
                    let
                        goban =
                            Goban 19 []

                        coords =
                            ( 3, 3 )

                        newGoban =
                            placeStone goban coords
                    in
                    Expect.equal newGoban.moves [ { color = Black, coords = ( 3, 3 ) } ]
            , test "places white stone after black" <|
                \_ ->
                    let
                        goban =
                            Goban 19 [ { color = Black, coords = ( 3, 3 ) } ]

                        coords =
                            ( 4, 4 )

                        newGoban =
                            placeStone goban coords
                    in
                    Expect.equal newGoban.moves [ { color = Black, coords = ( 3, 3 ) }, { color = White, coords = ( 4, 4 ) } ]
            , test "places black stone after black and white" <|
                \_ ->
                    let
                        goban =
                            Goban 19 [ { color = Black, coords = ( 3, 3 ) }, { color = White, coords = ( 4, 4 ) } ]

                        coords =
                            ( 5, 5 )

                        newGoban =
                            placeStone goban coords
                    in
                    Expect.equal newGoban.moves [ { color = Black, coords = ( 3, 3 ) }, { color = White, coords = ( 4, 4 ) }, { color = Black, coords = ( 5, 5 ) } ]
            ]
        , describe "getAdjacentPoints"
            [ test "for a stone in the center of the board returns 4 points" <|
                \_ ->
                    let
                        gobanSize =
                            19

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
                    Expect.equal (getAdjacentPoints gobanSize coords) expected
            , test "for a stone in the corner of the board returns 2 points" <|
                \_ ->
                    let
                        gobanSize =
                            19

                        coords =
                            ( 0, 0 )

                        expected =
                            Set.fromList
                                [ ( 1, 0 )
                                , ( 0, 1 )
                                ]
                    in
                    Expect.equal (getAdjacentPoints gobanSize coords) expected
            , test "for a stone on the edge of the board returns 3 points" <|
                \_ ->
                    let
                        gobanSize =
                            19

                        coords =
                            ( 0, 1 )

                        expected =
                            Set.fromList
                                [ ( 0, 0 )
                                , ( 0, 2 )
                                , ( 1, 1 )
                                ]
                    in
                    Expect.equal (getAdjacentPoints gobanSize coords) expected
            ]
        , describe "isAdjacent"
            [ test "adjacent horizontally" <|
                \_ ->
                    Expect.equal (isAdjacent 19 ( 3, 3 ) ( 3, 4 )) True
            , test "adjacent vertically" <|
                \_ ->
                    Expect.equal (isAdjacent 19 ( 3, 3 ) ( 4, 3 )) True
            , test "not adjacent diagonally" <|
                \_ ->
                    Expect.equal (isAdjacent 19 ( 3, 3 ) ( 4, 4 )) False
            , test "not adjacent far away" <|
                \_ ->
                    Expect.equal (isAdjacent 19 ( 3, 3 ) ( 10, 10 )) False
            , test "adjacent at edge" <|
                \_ ->
                    Expect.equal (isAdjacent 19 ( 0, 0 ) ( 0, 1 )) True
            ]
        , describe "belongsTo"
            [ test "stone adjacent to group (horizontal)" <|
                \_ ->
                    let
                        group =
                            Set.fromList [ ( 3, 3 ), ( 3, 4 ) ]

                        coords =
                            ( 3, 5 )
                    in
                    Expect.equal (belongsTo 19 group coords) True
            , test "stone adjacent to group (vertical)" <|
                \_ ->
                    let
                        group =
                            Set.fromList [ ( 3, 3 ), ( 4, 3 ) ]

                        coords =
                            ( 5, 3 )
                    in
                    Expect.equal (belongsTo 19 group coords) True
            , test "stone not adjacent to group" <|
                \_ ->
                    let
                        group =
                            Set.fromList [ ( 3, 3 ), ( 3, 4 ) ]

                        coords =
                            ( 5, 5 )
                    in
                    Expect.equal (belongsTo 19 group coords) False
            , test "stone adjacent to group at the edge of the board" <|
                \_ ->
                    let
                        group =
                            Set.fromList [ ( 0, 0 ) ]

                        coords =
                            ( 0, 1 )
                    in
                    Expect.equal (belongsTo 19 group coords) True
            , test "stone diagonally adjacent does not belong to group" <|
                \_ ->
                    let
                        group =
                            Set.fromList [ ( 3, 3 ) ]

                        coords =
                            ( 4, 4 )
                    in
                    Expect.equal (belongsTo 19 group coords) False
            ]
        , describe "allGroups"
            [ test "single stone forms a group" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ) ]
                            }

                        groups =
                            allGroups situation Black
                    in
                    Expect.equal groups [ Set.fromList [ ( 3, 3 ) ] ]
            , test "adjacent stones form one group" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ), ( ( 3, 4 ), Black ) ]
                            }

                        groups =
                            allGroups situation Black
                    in
                    Expect.equal groups [ Set.fromList [ ( 3, 3 ), ( 3, 4 ) ] ]
            , test "non-adjacent stones form separate groups" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ), ( ( 5, 5 ), Black ) ]
                            }

                        groups =
                            allGroups situation Black
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length groups) 2
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 3, 3 ) ]) groups) True
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 5, 5 ) ]) groups) True
                        ]
                        ()
            , test "diagonally adjacent stones form separate groups" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ), ( ( 4, 4 ), Black ) ]
                            }

                        groups =
                            allGroups situation Black
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length groups) 2
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 3, 3 ) ]) groups) True
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 4, 4 ) ]) groups) True
                        ]
                        ()
            , test "groups for only the given color are returned" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ), ( ( 3, 4 ), White ), ( ( 4, 4 ), Black ) ]
                            }

                        groups =
                            allGroups situation Black
                    in
                    Expect.all
                        [ \_ -> Expect.equal (List.length groups) 2
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 3, 3 ) ]) groups) True
                        , \_ -> Expect.equal (List.member (Set.fromList [ ( 4, 4 ) ]) groups) True
                        ]
                        ()
            ]
        , describe "stoneAt"
            [ test "returns Nothing for empty position" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.empty
                            }
                    in
                    Expect.equal (stoneAt situation ( 3, 3 )) Nothing
            , test "returns color for occupied position (Black)" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ) ]
                            }
                    in
                    Expect.equal (stoneAt situation ( 3, 3 )) (Just Black)
            , test "returns color for occupied position (White)" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 4, 4 ), White ) ]
                            }
                    in
                    Expect.equal (stoneAt situation ( 4, 4 )) (Just White)
            , test "returns Nothing for non-occupied position when others are occupied" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 9
                            , stones = Dict.fromList [ ( ( 3, 3 ), Black ), ( ( 4, 4 ), White ) ]
                            }
                    in
                    Expect.equal (stoneAt situation ( 5, 5 )) Nothing
            ]
        , describe "numLiberties"
            [ test "single stone in center has 4 liberties" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ) ]
                    in
                    Expect.equal (numLiberties situation group) 4
            , test "single stone in corner has 2 liberties" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 0, 0 ), Black ) ]
                            }

                        group =
                            Set.fromList [ ( 0, 0 ) ]
                    in
                    Expect.equal (numLiberties situation group) 2
            , test "single stone on edge has 3 liberties" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 0, 4 ), Black ) ]
                            }

                        group =
                            Set.fromList [ ( 0, 4 ) ]
                    in
                    Expect.equal (numLiberties situation group) 3
            , test "two adjacent stones share liberties" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ), ( ( 4, 5 ), Black ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ), ( 4, 5 ) ]
                    in
                    Expect.equal (numLiberties situation group) 6
            , test "liberties blocked by other stones" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ), ( ( 4, 5 ), White ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ) ]
                    in
                    Expect.equal (numLiberties situation group) 3
            ]
        , describe "isAlive"
            [ test "single stone with liberties is alive" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ) ]
                    in
                    Expect.equal (isAlive situation group) True
            , test "single stone with no liberties is not alive" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ), ( ( 4, 5 ), White ), ( ( 4, 3 ), White ), ( ( 3, 4 ), White ), ( ( 5, 4 ), White ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ) ]
                    in
                    Expect.equal (isAlive situation group) False
            , test "group with at least one liberty is alive" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ), ( ( 4, 5 ), Black ), ( ( 4, 3 ), White ), ( ( 3, 4 ), White ), ( ( 5, 4 ), White ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ), ( 4, 5 ) ]
                    in
                    Expect.equal (isAlive situation group) True
            , test "group with no liberties is not alive" <|
                \_ ->
                    let
                        situation =
                            { gobanSize = 19
                            , stones = Dict.fromList [ ( ( 4, 4 ), Black ), ( ( 4, 5 ), Black ), ( ( 4, 3 ), White ), ( ( 3, 4 ), White ), ( ( 5, 4 ), White ), ( ( 4, 6 ), White ), ( ( 5, 5 ), White ), ( ( 3, 5 ), White ) ]
                            }

                        group =
                            Set.fromList [ ( 4, 4 ), ( 4, 5 ) ]
                    in
                    Expect.equal (isAlive situation group) False
            ]
        , describe "undoMove"
            [ test "on empty goban does nothing" <|
                \_ ->
                    let
                        goban = Goban 19 []
                    in
                    Expect.equal (undoMove goban) goban

            , test "removes the last move" <|
                \_ ->
                    let
                        move1 = { color = Black, coords = (3, 4) }
                        move2 = { color = White, coords = (5, 3) }
                        goban = Goban 19 [ move1, move2 ]
                    in
                    Expect.equal (undoMove goban) (Goban 19 [ move1 ])

            , test "on goban with one move results in empty board" <|
                \_ ->
                    let
                        goban = Goban 19 [ { color = Black, coords = (3, 3) } ]
                    in
                    Expect.equal (undoMove goban) (Goban 19 [])
            ]
        , describe "opponent"
            [ test "opponent of Black is White" <|
                \_ ->
                    Expect.equal (opponent Black) White
            , test "opponent of White is Black" <|
                \_ ->
                    Expect.equal (opponent White) Black
            ]
        ]
