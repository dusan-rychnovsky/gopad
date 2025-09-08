-- elm-format src/Main.elm --yes
-- elm-test
-- elm make src/Main.elm --output=elm.js


module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Goban exposing (Goban)
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, src, style, type_)
import Html.Events exposing (onClick)
import Json.Decode


boardSize : Int
boardSize =
    19


gobanImgSize : Int
gobanImgSize =
    900


stoneImgSize : Int
stoneImgSize =
    35


type alias Game =
    { name : String
    , whitePlayer : String
    , blackPlayer : String
    , date : String
    , goban : Goban
    }


type alias Model =
    Game


type Msg
    = GobanClicked ( Int, Int )
    | UndoMove


main =
    Browser.sandbox
        { init =
            { name = ""
            , whitePlayer = ""
            , blackPlayer = ""
            , date = ""
            , goban = { size = boardSize, moves = [] }
            }
        , update = update
        , view = view
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        GobanClicked ( posX, posY ) ->
            let
                coords =
                    Goban.posToCoords model.goban posX posY gobanImgSize

                newGoban =
                    Goban.placeStone model.goban coords
            in
            Debug.log "Model" { model | goban = newGoban }

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
            { model | goban = newGoban }


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
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text "Save Game" ]
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text "Load Game" ]
                    , button [ type_ "button", Html.Attributes.disabled True ] [ text "New Game" ]
                    ]
                , div [ class "form-row form-row-narrow-gap" ]
                    [ label [ class "label label-game" ] [ text "Game Name", input [ type_ "text", class "input input-full" ] [] ]
                    , label [ class "label label-date" ] [ text "Date", input [ type_ "text", class "input input-date-long" ] [] ]
                    ]
                , div [ class "form-row" ]
                    [ label [ class "label" ] [ text "White Player", input [ type_ "text", class "input" ] [] ]
                    , label [ class "label" ] [ text "Black Player", input [ type_ "text", class "input" ] [] ]
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
