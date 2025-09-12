module Persist exposing (decodeGame, encodeGame)

import Game exposing (Game)
import Goban exposing (Color(..), Coords, Goban, Move)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "name", Encode.string game.name )
        , ( "whitePlayer", Encode.string game.whitePlayer )
        , ( "blackPlayer", Encode.string game.blackPlayer )
        , ( "date", Encode.string game.date )
        , ( "goban", encodeGoban game.goban )
        ]


decodeGame : Decode.Decoder Game
decodeGame =
    Decode.map5 Game
        (Decode.field "name" Decode.string)
        (Decode.field "whitePlayer" Decode.string)
        (Decode.field "blackPlayer" Decode.string)
        (Decode.field "date" Decode.string)
        (Decode.field "goban" decodeGoban)


encodeColor : Color -> Encode.Value
encodeColor color =
    case color of
        White ->
            Encode.string "White"

        Black ->
            Encode.string "Black"


decodeColor : Decode.Decoder Color
decodeColor =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "White" ->
                        Decode.succeed White

                    "Black" ->
                        Decode.succeed Black

                    _ ->
                        Decode.fail ("Unknown color: " ++ str)
            )


encodeCoords : Coords -> Encode.Value
encodeCoords ( row, col ) =
    Encode.list Encode.int [ row, col ]


decodeCoords : Decode.Decoder Coords
decodeCoords =
    Decode.list Decode.int
        |> Decode.andThen
            (\lst ->
                case lst of
                    [ row, col ] ->
                        Decode.succeed ( row, col )

                    _ ->
                        Decode.fail "Coords must be a list of two ints"
            )


encodeMove : Move -> Encode.Value
encodeMove move =
    Encode.object
        [ ( "color", encodeColor move.color )
        , ( "coords", encodeCoords move.coords )
        ]


decodeMove : Decode.Decoder Move
decodeMove =
    Decode.map2 Move
        (Decode.field "color" decodeColor)
        (Decode.field "coords" decodeCoords)


encodeGoban : Goban -> Encode.Value
encodeGoban goban =
    Encode.object
        [ ( "size", Encode.int goban.size )
        , ( "moves", Encode.list encodeMove goban.moves )
        ]


decodeGoban : Decode.Decoder Goban
decodeGoban =
    Decode.map2 Goban
        (Decode.field "size" Decode.int)
        (Decode.field "moves" (Decode.list decodeMove))
