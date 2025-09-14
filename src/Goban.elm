module Goban exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Goban =
    { size : Int
    , moves : Array Move
    }


type alias Move =
    { color : Color
    , coords : Coords
    }


type Color
    = White
    | Black


type alias Row =
    Int


type alias Col =
    Int


type alias Coords =
    ( Row, Col )


type alias Situation =
    { gobanSize : Int
    , stones : Dict Coords Color
    }


type alias Group =
    Set Coords


gobanImg =
    { sizePx = 2000.0
    , paddingPx = 60.0
    , squarePx = 99.0
    , linePx = 5.0
    }


empty : Int -> Goban
empty size =
    { size = size
    , moves = Array.empty
    }


isEmpty : Goban -> Bool
isEmpty goban =
    Array.isEmpty goban.moves


posToCoords : Goban -> Int -> Int -> Int -> Coords
posToCoords goban posX posY imgSize =
    let
        scale =
            gobanImg.sizePx / toFloat imgSize

        x =
            toFloat posX * scale

        y =
            toFloat posY * scale

        step =
            gobanImg.squarePx + gobanImg.linePx

        clamp v =
            if v < 0 then
                0

            else if v > goban.size - 1 then
                goban.size - 1

            else
                v

        col =
            clamp (round ((x - gobanImg.paddingPx) / step))

        row =
            clamp (round ((y - gobanImg.paddingPx) / step))
    in
    ( row, col )


coordsToPos : Goban -> Coords -> Int -> ( Int, Int )
coordsToPos goban ( row, col ) imgSize =
    let
        scale =
            toFloat imgSize / gobanImg.sizePx

        step =
            gobanImg.squarePx + gobanImg.linePx

        x =
            (gobanImg.paddingPx + toFloat col * step) * scale

        y =
            (gobanImg.paddingPx + toFloat row * step) * scale
    in
    ( round x, round y )


placeStone : Goban -> Coords -> Goban
placeStone goban coords =
    let
        newMove =
            { color = currentPlayer goban
            , coords = coords
            }
    in
    { goban | moves = Array.push newMove goban.moves }


undoMove : Goban -> Goban
undoMove goban =
    { goban | moves = Array.slice 0 (Array.length goban.moves - 1) goban.moves }


currentPlayer : Goban -> Color
currentPlayer goban =
    case Array.get (Array.length goban.moves - 1) goban.moves of
        Just lastMove ->
            opponent lastMove.color

        Nothing ->
            Black


getAdjacentPoints : Int -> Coords -> Set Coords
getAdjacentPoints gobanSize ( row, col ) =
    let
        candidates =
            [ ( row - 1, col )
            , ( row + 1, col )
            , ( row, col - 1 )
            , ( row, col + 1 )
            ]
    in
    candidates
        |> List.filter (\( r, c ) -> r >= 0 && c >= 0 && r < gobanSize && c < gobanSize)
        |> Set.fromList


isAdjacent : Int -> Coords -> Coords -> Bool
isAdjacent gobanSize first second =
    getAdjacentPoints gobanSize first |> Set.member second


belongsTo : Int -> Group -> Coords -> Bool
belongsTo gobanSize group coords =
    Set.foldl (\c acc -> acc || isAdjacent gobanSize c coords) False group


currentSituation : Goban -> Situation
currentSituation goban =
    Array.foldl applyMove { gobanSize = goban.size, stones = Dict.empty } goban.moves


applyMove : Move -> Situation -> Situation
applyMove move situation =
    let
        newSituation =
            { situation | stones = Dict.insert move.coords move.color situation.stones }

        opponentsGroups =
            allGroups newSituation (opponent move.color)

        capturedGroups =
            List.filter (\group -> not (isAlive newSituation group)) opponentsGroups

        stonesToRemove =
            List.concatMap Set.toList capturedGroups

        newStones =
            List.foldl Dict.remove newSituation.stones stonesToRemove
    in
    { situation | stones = newStones }


opponent : Color -> Color
opponent color =
    case color of
        White ->
            Black

        Black ->
            White


allGroups : Situation -> Color -> List Group
allGroups situation color =
    let
        addStone coords groups =
            let
                ( adjacentGroups, othersGroups ) =
                    List.partition (\group -> belongsTo situation.gobanSize group coords) groups

                merged =
                    List.foldl Set.union (Set.singleton coords) adjacentGroups
            in
            merged :: othersGroups
    in
    Dict.toList situation.stones
        |> List.filter (\( _, c ) -> c == color)
        |> List.map Tuple.first
        |> List.foldl addStone []


stoneAt : Situation -> Coords -> Maybe Color
stoneAt situation coords =
    Dict.get coords situation.stones


numLiberties : Situation -> Group -> Int
numLiberties situation group =
    group
        |> Set.foldl (\coords acc -> Set.union acc (getAdjacentPoints situation.gobanSize coords)) Set.empty
        |> Set.filter (\pt -> stoneAt situation pt == Nothing)
        |> Set.size


isAlive : Situation -> Group -> Bool
isAlive situation group =
    numLiberties situation group > 0
