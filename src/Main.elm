-- elm-format src/ tests/ --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


port module Main exposing (main)

import Browser
import DateFormat
import Dict exposing (Dict)
import Game exposing (Game)
import Goban exposing (Goban)
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, disabled, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import Sgf exposing (toSgf)
import Task
import Time exposing (Posix)


port downloadFile : { fileName : String, fileContent : String } -> Cmd msg


boardSize : Int
boardSize =
    19


gobanImgSize : Int
gobanImgSize =
    900


stoneImgSize : Int
stoneImgSize =
    35


type alias Model =
    Game


type Msg
    = InitTime Posix Time.Zone
    | UpdateName String
    | UpdateDate String
    | UpdateWhitePlayer String
    | UpdateBlackPlayer String
    | GobanClicked ( Int, Int )
    | UndoMove
    | SaveGame


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { name = ""
      , whitePlayer = ""
      , blackPlayer = ""
      , date = ""
      , goban = { size = boardSize, moves = [] }
      }
    , Task.perform (\( posix, zone ) -> InitTime posix zone) (Task.map2 Tuple.pair Time.now Time.here)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitTime time zone ->
            let
                newDate =
                    if String.trim model.date == "" then
                        DateFormat.formatDate time zone

                    else
                        model.date
            in
            ( { model | date = newDate }, Cmd.none )

        GobanClicked ( posX, posY ) ->
            let
                coords =
                    Goban.posToCoords model.goban posX posY gobanImgSize

                newGoban =
                    Goban.placeStone model.goban coords
            in
            ( Debug.log "Model" { model | goban = newGoban }, Cmd.none )

        UndoMove ->
            let
                moves =
                    model.goban.moves

                newMoves =
                    if List.isEmpty moves then
                        []

                    else
                        List.take (List.length moves - 1) moves

                goban =
                    model.goban

                newGoban =
                    { goban | moves = newMoves }
            in
            ( { model | goban = newGoban }, Cmd.none )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateDate date ->
            ( { model | date = date }, Cmd.none )

        UpdateWhitePlayer player ->
            ( { model | whitePlayer = player }, Cmd.none )

        UpdateBlackPlayer player ->
            ( { model | blackPlayer = player }, Cmd.none )

        SaveGame ->
            let
                ( fileName, fileContent ) =
                    toSgf model
            in
            ( model, downloadFile { fileName = fileName, fileContent = fileContent } )


view : Model -> Html Msg
view model =
    div [ class "gopad" ]
        [ h1 [ class "header" ] [ text "GOPAD" ]
        , div [ class "form-container" ]
            [ form [ class "form" ]
                [ div [ class "form-row form-row-buttons" ]
                    [ button
                        [ type_ "button"
                        , if List.isEmpty model.goban.moves then
                            disabled True

                          else
                            onClick UndoMove
                        ]
                        [ text "<" ]
                    , button [ type_ "button", disabled True ] [ text ">" ]
                    , button [ type_ "button", onClick SaveGame ] [ text "Save Game" ]
                    , button [ type_ "button", disabled True ] [ text "Load Game" ]
                    , button [ type_ "button", disabled True ] [ text "New Game" ]
                    ]
                , div [ class "form-row form-row-narrow-gap" ]
                    [ label [ class "label label-game" ]
                        [ text "Game Name"
                        , input
                            [ type_ "text"
                            , class "input input-full"
                            , value model.name
                            , onInput UpdateName
                            ]
                            []
                        ]
                    , label [ class "label label-date" ]
                        [ text "Date"
                        , input
                            [ type_ "text"
                            , class "input input-date-long"
                            , value model.date
                            , onInput UpdateDate
                            ]
                            []
                        ]
                    ]
                , div [ class "form-row" ]
                    [ label [ class "label" ]
                        [ text "White Player"
                        , input
                            [ type_ "text"
                            , class "input"
                            , value model.whitePlayer
                            , onInput UpdateWhitePlayer
                            ]
                            []
                        ]
                    , label [ class "label" ]
                        [ text "Black Player"
                        , input
                            [ type_ "text"
                            , class "input"
                            , value model.blackPlayer
                            , onInput UpdateBlackPlayer
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , div [ style "position" "relative", style "width" (String.fromInt gobanImgSize ++ "px"), style "height" (String.fromInt gobanImgSize ++ "px") ]
            ([ img
                [ src "public/goban.png"
                , class "goban-img"
                , alt "Goban."
                , style "width" (String.fromInt gobanImgSize ++ "px")
                , style "height" (String.fromInt gobanImgSize ++ "px")
                , on "click"
                    (Json.Decode.map2 (\x y -> GobanClicked ( x, y ))
                        (Json.Decode.field "offsetX" Json.Decode.int)
                        (Json.Decode.field "offsetY" Json.Decode.int)
                    )
                ]
                []
             ]
                ++ (Goban.currentSituation model.goban
                        |> .stones
                        |> Dict.toList
                        |> List.map
                            (\( coords, color ) ->
                                let
                                    ( posX, posY ) =
                                        Goban.coordsToPos model.goban coords gobanImgSize

                                    stoneSrc =
                                        case color of
                                            Goban.Black ->
                                                "public/black-stone.png"

                                            Goban.White ->
                                                "public/white-stone.png"
                                in
                                img
                                    [ src stoneSrc
                                    , alt "stone"
                                    , style "position" "absolute"
                                    , style "left" (String.fromInt (posX - stoneImgSize // 2) ++ "px")
                                    , style "top" (String.fromInt (posY - stoneImgSize // 2) ++ "px")
                                    , style "width" (String.fromInt stoneImgSize ++ "px")
                                    , style "height" (String.fromInt stoneImgSize ++ "px")
                                    , style "pointer-events" "none"
                                    ]
                                    []
                            )
                   )
            )
        ]
