-- elm-format src/ tests/ --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Game exposing (Game)
import Goban exposing (Goban)
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, src, style, type_)
import Html.Events exposing (onClick)
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
    = CurrentTime Posix
    | TimeZone Time.Zone
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
      , posix = Nothing
      , timeZone = Nothing
      , goban = { size = boardSize, moves = [] }
      }
    , Cmd.batch
        [ Task.perform CurrentTime Time.now
        , Task.perform TimeZone Time.here
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CurrentTime time ->
            ( { model | posix = Just time }, Cmd.none )

        TimeZone zone ->
            ( { model | timeZone = Just zone }, Cmd.none )

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
                            Html.Attributes.disabled True

                          else
                            Html.Events.onClick UndoMove
                        ]
                        [ text "<" ]
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text ">" ]
                    , button [ type_ "button", Html.Events.onClick SaveGame ] [ text "Save Game" ]
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text "Load Game" ]
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text "New Game" ]
                    ]
                , div [ class "form-row form-row-narrow-gap" ]
                    [ label [ class "label label-game" ]
                        [ text "Game Name"
                        , input
                            [ type_ "text"
                            , class "input input-full"
                            , Html.Attributes.value model.name
                            , Html.Events.onInput UpdateName
                            ]
                            []
                        ]
                    , label [ class "label label-date" ]
                        [ text "Date"
                        , input
                            [ type_ "text"
                            , class "input input-date-long"
                            , Html.Attributes.value (Game.dateStr model)
                            , Html.Events.onInput UpdateDate
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
                            , Html.Attributes.value model.whitePlayer
                            , Html.Events.onInput UpdateWhitePlayer
                            ]
                            []
                        ]
                    , label [ class "label" ]
                        [ text "Black Player"
                        , input
                            [ type_ "text"
                            , class "input"
                            , Html.Attributes.value model.blackPlayer
                            , Html.Events.onInput UpdateBlackPlayer
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
                , alt "Goban board"
                , style "width" (String.fromInt gobanImgSize ++ "px")
                , style "height" (String.fromInt gobanImgSize ++ "px")
                , Html.Events.on "click"
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
