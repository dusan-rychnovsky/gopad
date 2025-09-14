-- elm-format src/ tests/ --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


port module Main exposing (gameHistoryContent, main)

import Array exposing (Array)
import Browser
import DateFormat
import Dict exposing (Dict)
import Game exposing (Game)
import Goban exposing (Goban)
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, disabled, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Persist
import Sgf exposing (toSgf)
import Task
import Time exposing (Posix)


port downloadFile : { fileName : String, fileContent : String } -> Cmd msg


port storeState : Encode.Value -> Cmd msg


boardSize : Int
boardSize =
    19


gobanImgSize : Int
gobanImgSize =
    695


stoneImgSize : Int
stoneImgSize =
    33


gameHistoryTextAreaSize : { width : Int, height : Int }
gameHistoryTextAreaSize =
    { width = 100, height = gobanImgSize }


pageWidth : Int
pageWidth =
    gobanImgSize + 5 + gameHistoryTextAreaSize.width


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
    | NewGame


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateAndStoreState
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        model =
            case Decode.decodeValue Persist.decodeGame flags of
                Ok game ->
                    game

                Err _ ->
                    Game.emptyGame boardSize
    in
    ( model, triggerInitTime )


triggerInitTime : Cmd Msg
triggerInitTime =
    Task.perform (\( posix, zone ) -> InitTime posix zone) (Task.map2 Tuple.pair Time.now Time.here)


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
            ( { model | goban = Goban.undoMove model.goban }, Cmd.none )

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

        NewGame ->
            ( Game.emptyGame boardSize, triggerInitTime )


updateAndStoreState : Msg -> Model -> ( Model, Cmd Msg )
updateAndStoreState msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ storeState (Persist.encodeGame newModel), cmds ]
    )


view : Model -> Html Msg
view model =
    div [ class "gopad" ]
        [ h1 [ class "header" ] [ text "GOPAD" ]
        , div [ class "form-container", style "width" (String.fromInt pageWidth ++ "px") ]
            [ form [ class "form" ]
                [ div [ class "form-row form-row-buttons" ]
                    [ button
                        [ type_ "button"
                        , if Goban.isEmpty model.goban then
                            disabled True

                          else
                            onClick UndoMove
                        ]
                        [ text "<" ]
                    , button [ type_ "button", disabled True ] [ text ">" ]
                    , button [ type_ "button", onClick SaveGame ] [ text "Save Game" ]
                    , button [ type_ "button", disabled True ] [ text "Load Game" ]
                    , button [ type_ "button", onClick NewGame ] [ text "New Game" ]
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
        , div [ class "game-history-container" ]
            [ div [ style "position" "relative", style "width" (String.fromInt gobanImgSize ++ "px"), style "height" (String.fromInt gobanImgSize ++ "px") ]
                ([ img
                    [ src "public/goban.png"
                    , class "goban-img"
                    , alt "Goban."
                    , style "width" (String.fromInt gobanImgSize ++ "px")
                    , style "height" (String.fromInt gobanImgSize ++ "px")
                    , on "click"
                        (Decode.map2 (\x y -> GobanClicked ( x, y ))
                            (Decode.field "offsetX" Decode.int)
                            (Decode.field "offsetY" Decode.int)
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
            , Html.textarea
                [ Html.Attributes.class "game-history-textarea"
                , Html.Attributes.style "height" (String.fromInt gameHistoryTextAreaSize.height ++ "px")
                , Html.Attributes.style "width" (String.fromInt gameHistoryTextAreaSize.width ++ "px")
                , Html.Attributes.readonly True
                , Html.Attributes.value (gameHistoryContent model.goban)
                ]
                []
            ]
        ]


gameHistoryContent : Goban -> String
gameHistoryContent goban =
    let
        moves =
            goban.moves
                |> Array.toList
                |> List.indexedMap
                    (\i move ->
                        String.padLeft 2 '0' (String.fromInt (i + 1)) ++ Sgf.moveToSgf move
                    )
    in
    "GAME:\n-----"
        ++ (if List.isEmpty moves then
                ""

            else
                "\n" ++ String.join "\n" moves
           )
