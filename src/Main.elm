-- elm-format src/ tests/ --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


port module Main exposing (main, viewGameHistoryContent)

import Array exposing (Array)
import Browser
import DateFormat
import Dict exposing (Dict)
import Game exposing (Game)
import Goban
import Goban.Types as Goban exposing (Goban)
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, disabled, readonly, src, style, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Persist
import Sgf exposing (toSgf)
import Task
import Time exposing (Posix)


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



-- PORTS


port downloadFile : { fileName : String, fileContent : String } -> Cmd msg


port storeState : Encode.Value -> Cmd msg



-- MODEL


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



-- MAIN


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



-- UPDATE


updateAndStoreState : Msg -> Model -> ( Model, Cmd Msg )
updateAndStoreState msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ storeState (Persist.encodeGame newModel.game), cmds ]
    )


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
            ( { model | game = { game | date = updateDate game.date time zone } }, Cmd.none )

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


updateDate : String -> Posix -> Time.Zone -> String
updateDate currentDate time zone =
    if String.trim currentDate == "" then
        DateFormat.formatDate time zone

    else
        currentDate



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "gopad" ]
        [ h1 [ class "header" ] [ text "GOPAD" ]
        , viewForm model.game
        , div [ class "game-container" ]
            [ viewGoban model.game.goban
            , viewRightPane model.game.goban model.placingHandicapMode
            ]
        ]


viewForm : Game -> Html Msg
viewForm game =
    div [ class "form-container", style "width" (String.fromInt pageWidth ++ "px") ]
        [ form [ class "form" ]
            [ div [ class "form-row form-row-buttons" ]
                [ button
                    [ type_ "button"
                    , if Goban.isEmpty game.goban then
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


viewGoban : Goban -> Html Msg
viewGoban goban =
    div [ style "position" "relative", style "width" (String.fromInt gobanImgSize ++ "px"), style "height" (String.fromInt gobanImgSize ++ "px") ]
        (viewGobanImg ++ viewStones goban)


viewGobanImg : List (Html Msg)
viewGobanImg =
    [ img
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


viewStones : Goban -> List (Html Msg)
viewStones goban =
    Goban.currentSituation goban
        |> .stones
        |> Dict.toList
        |> List.map viewStone


viewStone : ( Goban.Coords, Goban.Color ) -> Html Msg
viewStone ( coords, color ) =
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


viewRightPane : Goban -> Bool -> Html Msg
viewRightPane goban placingHandicapMode =
    div [ class "right-pane" ]
        [ viewToggleHandicapButton placingHandicapMode
        , viewGameHistoryTextArea goban
        ]


viewToggleHandicapButton : Bool -> Html Msg
viewToggleHandicapButton placingHandicapMode =
    button
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


viewGameHistoryTextArea : Goban -> Html Msg
viewGameHistoryTextArea goban =
    Html.textarea
        [ class "game-history-textarea"
        , style "height" (String.fromInt (rightPaneSize.height - handicapButtonHeight) ++ "px")
        , style "width" (String.fromInt rightPaneSize.width ++ "px")
        , readonly True
        , value (viewGameHistoryContent goban)
        ]
        []


viewGameHistoryContent : Goban -> String
viewGameHistoryContent goban =
    let
        situation =
            Goban.currentSituation goban

        whiteCaptures =
            Goban.numCaptured situation Goban.White

        blackCaptures =
            Goban.numCaptured situation Goban.Black

        textCaptures =
            "B: "
                ++ String.fromInt blackCaptures
                ++ "\n"
                ++ "W: "
                ++ String.fromInt whiteCaptures
                ++ "\n"

        textMoves =
            goban.moves
                |> Array.toList
                |> List.indexedMap
                    (\i move ->
                        String.padLeft 2 '0' (String.fromInt (i + 1)) ++ Sgf.moveToSgf move
                    )
                |> String.join "\n"
    in
    "GAME:\n"
        ++ "-----\n"
        ++ textCaptures
        ++ "-----\n"
        ++ textMoves
