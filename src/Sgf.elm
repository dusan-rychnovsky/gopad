module Sgf exposing (..)

-- https://www.red-bean.com/sgf/properties.html

import Array exposing (Array)
import Game exposing (Game)
import Goban.Types as Goban


type alias FileName =
    String


type alias FileContent =
    String


toSgf : Game -> ( FileName, FileContent )
toSgf model =
    let
        gameInfo =
            ";FF[4]GM[1]AP[gopad:0.1]GN["
                ++ model.name
                ++ "]DT["
                ++ model.date
                ++ "]PB["
                ++ model.blackPlayer
                ++ "]PW["
                ++ model.whitePlayer
                ++ "]SZ["
                ++ String.fromInt model.goban.size
                ++ "]KM[6.5]"

        movesToSgf =
            model.goban.moves
                |> Array.map moveToSgf
                |> Array.toList
                |> String.join "\n"

        content =
            "(" ++ gameInfo ++ "\n" ++ movesToSgf ++ "\n)"
    in
    ( "game.sgf", content )


moveToSgf : Goban.Move -> String
moveToSgf move =
    let
        coordsCode n =
            String.fromChar (Char.fromCode (Char.toCode 'a' + n))

        colorCode color =
            case color of
                Goban.Black ->
                    "B"

                Goban.White ->
                    "W"

        ( row, col ) =
            move.coords
    in
    ";"
        ++ colorCode move.color
        ++ "["
        ++ coordsCode col
        ++ coordsCode row
        ++ "]"
