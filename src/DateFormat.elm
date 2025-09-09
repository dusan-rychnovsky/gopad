module DateFormat exposing (formatDate)

import Time exposing (Month, Posix, Zone)


formatDate : Posix -> Zone -> String
formatDate posix zone =
    let
        year =
            Time.toYear zone posix

        month =
            Time.toMonth zone posix |> monthToInt

        day =
            Time.toDay zone posix

        pad n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    String.fromInt year ++ "-" ++ pad month ++ "-" ++ pad day


monthToInt : Month -> Int
monthToInt m =
    case m of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
