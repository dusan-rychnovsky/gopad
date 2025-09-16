module Game exposing (Game, emptyGame)

import Goban
import Goban.Types exposing (Goban)


type alias Game =
    { name : String
    , whitePlayer : String
    , blackPlayer : String
    , date : String
    , goban : Goban
    }


emptyGame : Int -> Game
emptyGame boardSize =
    { name = ""
    , whitePlayer = ""
    , blackPlayer = ""
    , date = ""
    , goban = Goban.empty boardSize
    }
