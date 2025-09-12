module Game exposing (..)

import Goban exposing (Goban)


type alias Game =
    { name : String
    , whitePlayer : String
    , blackPlayer : String
    , date : String
    , goban : Goban
    }
