-- elm-format src/ tests/ --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


port module Main exposing (gameHistoryContent, main)

import Array exposing (Array)
import Browser
import DateFormat
import Dict exposing (Dict)
import Game exposing (Game)
import Goban
import Goban.Types as Goban exposing (Goban)
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


rightPaneSize : { width : Int, height : Int }
rightPaneSize =
    { width = 100, height = gobanImgSize }


handicapButtonHeight : Int
handicapButtonHeight =
    35


pageWidth : Int
pageWidth =
    gobanImgSize + 5 + rightPaneSize.width


type alias Model =
    { game : Game
    , placingHandicapMode : Bool
    }


type Msg
    = InitTime Posix Time.Zone
    | UpdateLocation String
    | UpdateDate String
    | UpdateWhitePlayer String
    | UpdateBlackPlayer String
    | GobanClicked ( Int, Int )
    | UndoMove
    | SaveGame
    | NewGame
    | ToggleHandicap


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
        game =
            case Decode.decodeValue Persist.decodeGame flags of
                Ok storedGame ->
                    storedGame

                Err _ ->
                    Game.emptyGame boardSize
    in
    ( { game = game
      , placingHandicapMode = False
      }
    , triggerInitTime
    )


triggerInitTime : Cmd Msg
triggerInitTime =
    Task.perform (\( posix, zone ) -> InitTime posix zone) (Task.map2 Tuple.pair Time.now Time.here)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( game, placingHandicapMode ) =
            ( model.game, model.placingHandicapMode )

        goban =
            game.goban
    in
    case msg of
        InitTime time zone ->
            let
                newDate =
                    if String.trim game.date == "" then
                        DateFormat.formatDate time zone

                    else
                        game.date
            in
            ( { model | game = { game | date = newDate } }, Cmd.none )

        GobanClicked ( posX, posY ) ->
            let
                coords =
                    Goban.posToCoords goban.size posX posY gobanImgSize

                newGoban =
                    if placingHandicapMode then
                        Goban.placeHandicapStone goban coords

                    else
                        Goban.placeStone goban coords
            in
            ( { model | game = { game | goban = newGoban } }, Cmd.none )

        UndoMove ->
            ( { model | game = { game | goban = Goban.undoMove goban } }, Cmd.none )

        UpdateDate date ->
            ( { model | game = { game | date = date } }, Cmd.none )

        UpdateBlackPlayer player ->
            ( { model | game = { game | blackPlayer = player } }, Cmd.none )

        UpdateWhitePlayer player ->
            ( { model | game = { game | whitePlayer = player } }, Cmd.none )

        UpdateLocation location ->
            ( { model | game = { game | location = location } }, Cmd.none )

        SaveGame ->
            let
                ( fileName, fileContent ) =
                    toSgf game
            in
            ( model, downloadFile { fileName = fileName, fileContent = fileContent } )

        NewGame ->
            ( { game = Game.emptyGame boardSize
              , placingHandicapMode = False
              }
            , triggerInitTime
            )

        ToggleHandicap ->
            ( { model | placingHandicapMode = not placingHandicapMode }, Cmd.none )


updateAndStoreState : Msg -> Model -> ( Model, Cmd Msg )
updateAndStoreState msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ storeState (Persist.encodeGame newModel.game), cmds ]
    )


view : Model -> Html Msg
view model =
    let
        ( game, placingHandicapMode ) =
            ( model.game, model.placingHandicapMode )

        goban =
            game.goban
    in
    div [ class "gopad" ]
        [ h1 [ class "header" ] [ text "GOPAD" ]
        , div [ class "form-container", style "width" (String.fromInt pageWidth ++ "px") ]
            [ form [ class "form" ]
                [ div [ class "form-row form-row-buttons" ]
                    [ button
                        [ type_ "button"
                        , if Goban.isEmpty goban then
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
                    [ label [ class "label label-date" ]
                        [ text "Date"
                        , input
                            [ type_ "text"
                            , class "input input-date"
                            , value game.date
                            , onInput UpdateDate
                            ]
                            []
                        ]
                    , label [ class "label label-black" ]
                        [ text "Black"
                        , input
                            [ type_ "text"
                            , class "input input-black"
                            , value game.blackPlayer
                            , onInput UpdateBlackPlayer
                            ]
                            []
                        ]
                    , label [ class "label label-white" ]
                        [ text "White"
                        , input
                            [ type_ "text"
                            , class "input input-white"
                            , value game.whitePlayer
                            , onInput UpdateWhitePlayer
                            ]
                            []
                        ]
                    , label [ class "label label-location" ]
                        [ text "Location"
                        , input
                            [ type_ "text"
                            , class "input input-location"
                            , value game.location
                            , onInput UpdateLocation
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , div [ class "game-container" ]
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
                    ++ (Goban.currentSituation goban
                            |> .stones
                            |> Dict.toList
                            |> List.map
                                (\( coords, color ) ->
                                    let
                                        ( posX, posY ) =
                                            Goban.coordsToPos coords gobanImgSize

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
            , div [ class "right-pane" ]
                [ button
                    [ type_ "button"
                    , class
                        (if placingHandicapMode then
                            "handicap-button toggled"

                         else
                            "handicap-button"
                        )
                    , style "width" (String.fromInt rightPaneSize.width ++ "px")
                    , onClick ToggleHandicap
                    ]
                    [ text "Handicap" ]
                , Html.textarea
                    [ Html.Attributes.class "game-history-textarea"
                    , Html.Attributes.style "height" (String.fromInt (rightPaneSize.height - handicapButtonHeight) ++ "px")
                    , Html.Attributes.style "width" (String.fromInt rightPaneSize.width ++ "px")
                    , Html.Attributes.readonly True
                    , Html.Attributes.value (gameHistoryContent goban)
                    ]
                    []
                ]
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
