module Sgf exposing (..)

-- https://www.red-bean.com/sgf/properties.html

import Array exposing (Array)
import Game exposing (Game)
import Goban.Types as Goban
import StringUtils


type alias FileName =
    String


type alias FileContent =
    String


toSgf : Game -> ( FileName, FileContent )
toSgf game =
    let
        gameInfo =
            ";FF[4]GM[1]AP[gopad:0.1]GN["
                ++ Game.name game
                ++ "]DT["
                ++ game.date
                ++ "]PB["
                ++ game.blackPlayer
                ++ "]PW["
                ++ game.whitePlayer
                ++ "]SZ["
                ++ String.fromInt game.goban.size
                ++ "]KM[6.5]"

        movesToSgf =
            game.goban.moves
                |> Array.map moveToSgf
                |> Array.toList
                |> String.join "\n"

        content =
            "(" ++ gameInfo ++ "\n" ++ movesToSgf ++ "\n)"
    in
    ( fileName game, content )


fileName : Game -> String
fileName game =
    let
        name =
            [ game.date, game.blackPlayer, game.whitePlayer, game.location ]
                |> StringUtils.joinTrimmed "-"
    in
    if name == "" then
        "game.sgf"

    else
        name ++ ".sgf"


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
