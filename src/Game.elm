module Game exposing (Game, emptyGame, name)

import Goban
import Goban.Types exposing (Goban)
import StringUtils


type alias Game =
    { date : String
    , blackPlayer : String
    , whitePlayer : String
    , location : String
    , placingHandicapMode : Bool
    , goban : Goban
    }


emptyGame boardSize =
    { date = ""
    , blackPlayer = ""
    , whitePlayer = ""
    , location = ""
    , placingHandicapMode = False
    , goban = Goban.empty boardSize
    }


name : Game -> String
name game =
    [ game.date, game.blackPlayer, game.whitePlayer, game.location ]
        |> StringUtils.joinTrimmed " "
