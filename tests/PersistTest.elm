module PersistTest exposing (suite)

import Array exposing (Array)
import Expect
import Game exposing (..)
import Goban.Types as Goban
import Json.Decode as Decode
import Json.Encode as Encode
import Persist exposing (decodeGame, encodeGame)
import Test exposing (..)


game : Game
game =
    { name = "Test Game"
    , whitePlayer = "Alice"
    , blackPlayer = "Bob"
    , date = "2025-09-12"
    , goban =
        { size = 9
        , moves =
            Array.fromList
                [ { color = Goban.Black, coords = ( 1, 1 ) }
                , { color = Goban.White, coords = ( 2, 2 ) }
                ]
        }
    }


suite : Test
suite =
    describe "Persist.encodeGame/.decodeGame"
        [ test "encodeGame then decodeGame returns original value" <|
            \_ ->
                Expect.equal (Ok game) (Decode.decodeValue decodeGame (encodeGame game))
        ]
