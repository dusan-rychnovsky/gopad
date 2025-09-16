module Goban.Internal exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Goban.Types exposing (..)
import Set exposing (Set)


currentPlayer : Goban -> Color
currentPlayer goban =
    case Array.get (Array.length goban.moves - 1) goban.moves of
        Just lastMove ->
            opponent lastMove.color

        Nothing ->
            Black


opponent : Color -> Color
opponent color =
    case color of
        White ->
            Black

        Black ->
            White


stoneAt : Situation -> Coords -> Maybe Color
stoneAt situation coords =
    Dict.get coords situation.stones


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


belongsTo : Int -> Group -> Coords -> Bool
belongsTo gobanSize group coords =
    Set.foldl (\c acc -> acc || isAdjacent gobanSize c coords) False group


isAdjacent : Int -> Coords -> Coords -> Bool
isAdjacent gobanSize first second =
    getAdjacentPoints gobanSize first |> Set.member second


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


isAlive : Situation -> Group -> Bool
isAlive situation group =
    numLiberties situation group > 0


numLiberties : Situation -> Group -> Int
numLiberties situation group =
    group
        |> Set.foldl (\coords acc -> Set.union acc (getAdjacentPoints situation.gobanSize coords)) Set.empty
        |> Set.filter (\pt -> stoneAt situation pt == Nothing)
        |> Set.size
