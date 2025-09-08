module Sgf exposing (..)

import Game exposing (Game)


type alias FileName =
    String


type alias FileContent =
    String


toSgf : Game -> ( FileName, FileContent )
toSgf model =
    ( "game.sgf", "Hello World!" )
