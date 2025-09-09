module Game exposing (..)

import DateFormat exposing (formatDate)
import Goban exposing (Goban)
import Time exposing (Posix)


type alias Game =
    { name : String
    , whitePlayer : String
    , blackPlayer : String
    , posix : Maybe Posix
    , timeZone : Maybe Time.Zone
    , date : String
    , goban : Goban
    }


dateStr : Game -> String
dateStr game =
    if String.trim game.date /= "" then
        game.date

    else
        case ( game.posix, game.timeZone ) of
            ( Just posix, Just zone ) ->
                formatDate posix zone

            _ ->
                ""
