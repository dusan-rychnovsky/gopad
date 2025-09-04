module GobanTest exposing (all)

import Goban exposing (..)

import Test exposing (..)
import Expect

all : Test
all =
    describe "Goban module"
        [ test "hello world" <|
            \_ ->
                Expect.equal (.size (Goban 19 [])) 19
        ]
