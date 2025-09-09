module DateFormatTest exposing (..)

import DateFormat exposing (formatDate)
import Expect
import Test exposing (..)
import Time exposing (millisToPosix, utc)


suite : Test
suite =
    describe "DateFormat.formatDate"
        [ test "formats 2025-09-09" <|
            \_ ->
                Expect.equal (formatDate (millisToPosix 1757376000000) utc) "2025-09-09"
        , test "formats 2025-01-01" <|
            \_ ->
                Expect.equal (formatDate (millisToPosix 1735689600000) utc) "2025-01-01"
        , test "formats 2025-12-31" <|
            \_ ->
                Expect.equal (formatDate (millisToPosix 1767139200000) utc) "2025-12-31"
        ]
