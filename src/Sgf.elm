module Sgf exposing (..)

import Game exposing (Game)
import Goban exposing (Color(..), Move)


type alias FileName =
    String


type alias FileContent =
    String


toSgf : Game -> ( FileName, FileContent )
toSgf model =
    let
        header =
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
            List.map moveToSgf model.goban.moves
                |> String.join "\n"

        content =
            "(" ++ header ++ "\n" ++ movesToSgf ++ "\n)"
    in
    ( "game.sgf", content )


moveToSgf : Move -> String
moveToSgf move =
    let
        coordsCode n =
            String.fromChar (Char.fromCode (Char.toCode 'a' + n))

        colorCode color =
            case color of
                Black ->
                    "B"

                White ->
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
