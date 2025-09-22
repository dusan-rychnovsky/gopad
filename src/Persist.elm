module Persist exposing (decodeGame, encodeGame)

import Array exposing (Array)
import Game exposing (Game)
import Goban.Types as Goban exposing (Goban)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)


encodeGame : Game -> Encode.Value
encodeGame game =
    Encode.object
        [ ( "date", Encode.string game.date )
        , ( "blackPlayer", Encode.string game.blackPlayer )
        , ( "whitePlayer", Encode.string game.whitePlayer )
        , ( "location", Encode.string game.location )
        , ( "goban", encodeGoban game.goban )
        ]


decodeGame : Decode.Decoder Game
decodeGame =
    Decode.map5 Game
        (Decode.field "date" Decode.string)
        (Decode.field "blackPlayer" Decode.string)
        (Decode.field "whitePlayer" Decode.string)
        (Decode.field "location" Decode.string)
        (Decode.field "goban" decodeGoban)


encodeColor : Goban.Color -> Encode.Value
encodeColor color =
    case color of
        Goban.White ->
            Encode.string "White"

        Goban.Black ->
            Encode.string "Black"


decodeColor : Decode.Decoder Goban.Color
decodeColor =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "White" ->
                        Decode.succeed Goban.White

                    "Black" ->
                        Decode.succeed Goban.Black

                    _ ->
                        Decode.fail ("Unknown color: " ++ str)
            )


encodeCoords : Goban.Coords -> Encode.Value
encodeCoords ( row, col ) =
    Encode.list Encode.int [ row, col ]


decodeCoords : Decode.Decoder Goban.Coords
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


encodeMove : Goban.Move -> Encode.Value
encodeMove move =
    Encode.object
        [ ( "color", encodeColor move.color )
        , ( "coords", encodeCoords move.coords )
        ]


decodeMove : Decode.Decoder Goban.Move
decodeMove =
    Decode.map2 Goban.Move
        (Decode.field "color" decodeColor)
        (Decode.field "coords" decodeCoords)


encodeGoban : Goban -> Encode.Value
encodeGoban goban =
    Encode.object
        [ ( "size", Encode.int goban.size )
        , ( "moves", Encode.list encodeMove (Array.toList goban.moves) )
        ]


decodeGoban : Decode.Decoder Goban
decodeGoban =
    Decode.map2 Goban
        (Decode.field "size" Decode.int)
        (Decode.field "moves" (Decode.list decodeMove |> Decode.map Array.fromList))
