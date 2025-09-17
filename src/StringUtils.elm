module StringUtils exposing (joinTrimmed)


joinTrimmed : String -> List String -> String
joinTrimmed delimiter strings =
    strings
        |> List.map String.trim
        |> List.filter (\s -> s /= "")
        |> String.join delimiter
        |> String.trim
