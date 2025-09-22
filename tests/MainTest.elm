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
                    "GAME:\n-----\nB: 0\nW: 0\n-----\n"
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
                    "GAME:\n-----\nB: 0\nW: 0\n-----\n01;B[ed]\n02;W[ck]"
                    (gameHistoryContent goban)
        , test "game with captures" <|
            \_ ->
                let
                    goban =
                        List.foldl (\coords g -> Goban.placeStone g coords)
                            (Goban.empty 9)
                            [ ( 2, 2 ) -- B
                            , ( 1, 2 ) -- W
                            , ( 2, 3 ) -- B
                            , ( 1, 3 ) -- W
                            , ( 3, 2 ) -- B
                            , ( 3, 3 ) -- W
                            , ( 2, 1 ) -- B
                            , ( 4, 4 ) -- W
                            , ( 2, 4 ) -- B - captures (1,2) and (1,3)
                            ]
                in
                Expect.equal
                    "GAME:\n-----\nB: 0\nW: 0\n-----\n01;B[cc]\n02;W[cb]\n03;B[dc]\n04;W[db]\n05;B[cd]\n06;W[dd]\n07;B[bc]\n08;W[ee]\n09;B[ec]"
                    (gameHistoryContent goban)
        ]
