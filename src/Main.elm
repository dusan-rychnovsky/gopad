-- elm-format src/Main.elm --yes
-- elm make src/Main.elm --output=elm.js


module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, form, h1, img, input, label, node, text)
import Html.Attributes exposing (alt, class, src, type_)
import Html.Events exposing (onClick)


type alias Model =
    ()


main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = view
        }


view : Model -> Html msg
view _ =
    div [ class "gopad" ]
        [ h1 [ class "header" ] [ text "GOPAD" ]
        , div [ class "form-container" ]
            [ form [ class "form" ]
                [ div [ class "form-row form-row-buttons" ]
                    [ button [ type_ "button" ] [ text "<" ]
                    , button [ type_ "button" ] [ text ">" ]
                    , button [ type_ "button" ] [ text "Save Game" ]
                    , button [ type_ "button" ] [ text "Load Game" ]
                    , button [ type_ "button" ] [ text "New Game" ]
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
        , img [ src "public/goban.png", class "goban-img", alt "Goban board" ] []
        ]
