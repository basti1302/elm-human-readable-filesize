module Tests exposing (all)

import Expect
import Filesize exposing (..)
import Fuzz exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Human Readable File Sizes"
        [ string_based
        , tuple_based
        ]


string_based : Test
string_based =
    describe "String-based cases"
        [ describe "Basic cases"
            [ test "3 B" <| \() -> Expect.equal "3 B" (format 3)
            , test "100 B" <| \() -> Expect.equal "100 B" (format 100)
            , test "543 B" <| \() -> Expect.equal "543 B" (format 543)
            , test "1.33 kB" <| \() -> Expect.equal "1.33 kB" (format 1337)
            , test "238.67 MB" <| \() -> Expect.equal "238.67 MB" (format 238674052)
            ]
        , describe "Edge cases"
            [ test "0 B" <| \() -> Expect.equal "0 B" (format 0)
            , test "1 B" <| \() -> Expect.equal "1 B" (format 1)
            , test "999 B" <| \() -> Expect.equal "999 B" (format 999)
            , test "1 kB" <| \() -> Expect.equal "1 kB" (format 1000)
            , test "999.99 kB" <| \() -> Expect.equal "999.99 kB" (format 999999)
            , test "1 MB" <| \() -> Expect.equal "1 MB" (format 1000000)
            , test "999.99 MB" <| \() -> Expect.equal "999.99 MB" (format 999999999)
            , test "1 GB" <| \() -> Expect.equal "1 GB" (format 1000000000)
            , test "999.99 GB" <| \() -> Expect.equal "999.99 GB" (format 999999999999)
            , test "1 TB" <| \() -> Expect.equal "1 TB" (format 1000000000000)
            , test "999.99 TB" <| \() -> Expect.equal "999.99 TB" (format 999999999999999)
            , test "1 PB" <| \() -> Expect.equal "1 PB" (format 1000000000000000)

            -- TODO For very large file sizes, the implementation of
            -- roundToDecimalPlaces can introduce rounding errors that lead to
            -- different results. Therefore, for now, we test with
            -- 999999999999999000 instead of 999999999999999999.
            , test "999.99 PB" <| \() -> Expect.equal "999.99 PB" (format 999999999999999000)
            , test "1 EB" <| \() -> Expect.equal "1 EB" (format 1000000000000000000)

            -- Somewhere around this (9223350000000000000) elm-format
            -- overwrites the Int literal with garbage, possibly due to an
            -- integer overflow. In order to keep this file editable with
            -- elm-format enabled, we do not use larger Ints in our tests.
            , test "922.33 EB" <| \() -> Expect.equal "9.22 EB" (format 9223350000000000000)
            ]
        , describe "Basic negative cases"
            [ test "-3 B" <| \() -> Expect.equal "-3 B" (format -3)
            , test "-100 B" <| \() -> Expect.equal "-100 B" (format -100)
            , test "-543 B" <| \() -> Expect.equal "-543 B" (format -543)
            , test "-1.33 kB" <| \() -> Expect.equal "-1.33 kB" (format -1337)
            , test "-238.67 MB" <| \() -> Expect.equal "-238.67 MB" (format -238674052)
            ]
        , describe "Negative edge cases"
            [ test "-1 B" <| \() -> Expect.equal "-1 B" (format -1)
            , test "-999 B" <| \() -> Expect.equal "-999 B" (format -999)
            , test "-1 kB" <| \() -> Expect.equal "-1 kB" (format -1000)
            , test "-999.99 kB" <| \() -> Expect.equal "-999.99 kB" (format -999999)
            , test "-1 MB" <| \() -> Expect.equal "-1 MB" (format -1000000)
            , test "-999.99 MB" <| \() -> Expect.equal "-999.99 MB" (format -999999999)
            , test "-1 GB" <| \() -> Expect.equal "-1 GB" (format -1000000000)
            , test "-999.99 GB" <| \() -> Expect.equal "-999.99 GB" (format -999999999999)
            , test "-1 TB" <| \() -> Expect.equal "-1 TB" (format -1000000000000)
            , test "-999.99 TB" <| \() -> Expect.equal "-999.99 TB" (format -999999999999999)
            , test "-1 PB" <| \() -> Expect.equal "-1 PB" (format -1000000000000000)
            , test "-999.99 PB" <| \() -> Expect.equal "-999.99 PB" (format -999999999999999000)
            , test "-1 EB" <| \() -> Expect.equal "-1 EB" (format -1000000000000000000)
            , test "-922.33 EB" <| \() -> Expect.equal "-9.22 EB" (format -9223350000000000000)
            ]
        , describe "Fuzzed cases"
            [ fuzz (intRange 0 999) "Positive byte range" <|
                \num ->
                    Expect.true "ends with \" B\""
                        (num |> format |> String.endsWith " B")
            , fuzz (intRange 0 999) "Positive byte range sign" <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> format |> String.startsWith "-" |> not)
            , fuzz (intRange -999 -1) "Negative byte range" <|
                \num ->
                    Expect.true "ends with \" B\""
                        (num |> format |> String.endsWith " B")
            , fuzz (intRange -999 -1) "Negative byte range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> format |> String.startsWith "-")
            , fuzz (intRange 1000 999999) "Positive kB range" <|
                \num ->
                    Expect.true "ends with \" kB\""
                        (num |> format |> String.endsWith " kB")
            , fuzz (intRange 1000 999999) "Positive kB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> format |> String.startsWith "-" |> not)
            , fuzz (intRange -999999 -1000) "Negative kB range" <|
                \num ->
                    Expect.true "ends with \" kB\""
                        (num |> format |> String.endsWith " kB")
            , fuzz (intRange -999999 -1000) "Negative kB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> format |> String.startsWith "-")
            , fuzz (intRange 1000000 999999999) "Positive MB range" <|
                \num ->
                    Expect.true "ends with \" MB\""
                        (num |> format |> String.endsWith " MB")
            , fuzz (intRange 1000000 999999999) "Positive MB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> format |> String.startsWith "-" |> not)
            , fuzz (intRange -999999999 -1000000) "Negative MB range" <|
                \num ->
                    Expect.true "ends with \" MB\""
                        (num |> format |> String.endsWith " MB")
            , fuzz (intRange -999999999 -1000000) "Negative MB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> format |> String.startsWith "-")
            , fuzz (intRange 1000000000 999999999999) "Positive GB range" <|
                \num ->
                    Expect.true "ends with \" GB\""
                        (num |> format |> String.endsWith " GB")
            , fuzz (intRange 1000000000 999999999999) "Positive GB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> format |> String.startsWith "-" |> not)
            , fuzz (intRange -999999999999 -1000000000) "Negative GB range" <|
                \num ->
                    Expect.true "ends with \" GB\""
                        (num |> format |> String.endsWith " GB")
            , fuzz (intRange -999999999999 -1000000000) "Negative GB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> format |> String.startsWith "-")
            ]
        , describe "With settings (decimalPlaces)"
            [ test "zero decimal places" <|
                \() ->
                    Expect.equal
                        "238 MB"
                        (formatWith { defaultSettings | decimalPlaces = 0 } 238674052)
            , test "one decimal place" <|
                \() ->
                    Expect.equal
                        "238.6 MB"
                        (formatWith { defaultSettings | decimalPlaces = 1 } 238674052)
            , test "three decimal places" <|
                \() ->
                    Expect.equal
                        "238.674 MB"
                        (formatWith { defaultSettings | decimalPlaces = 3 } 238674052)
            , test "four decimal places with trailing zero" <|
                \() ->
                    Expect.equal
                        "238.674 MB"
                        (formatWith { defaultSettings | decimalPlaces = 4 } 238674052)
            , test "238.67405 MB" <|
                \() ->
                    Expect.equal
                        "238.67405 MB"
                        (formatWith { defaultSettings | decimalPlaces = 5 } 238674052)
            , test "use negative decimal places to round 1" <|
                \() ->
                    Expect.equal
                        "230 MB"
                        (formatWith { defaultSettings | decimalPlaces = -1 } 238674052)
            , test "use negative decimal places to round 2" <|
                \() ->
                    Expect.equal
                        "200 MB"
                        (formatWith { defaultSettings | decimalPlaces = -2 } 238674052)
            ]
        , describe "With settings (decimalSeparator)"
            [ test "use comma as decimal separator" <|
                \() ->
                    Expect.equal
                        "238,67 MB"
                        (formatWith { defaultSettings | decimalSeparator = "," } 238674052)
            ]
        , describe "Base 2 basic cases"
            [ test "3 B" <| \() -> Expect.equal "3 B" (formatBase2 3)
            , test "100 B" <| \() -> Expect.equal "100 B" (formatBase2 100)
            , test "543 B" <| \() -> Expect.equal "543 B" (formatBase2 543)
            , test "1.61 KiB" <| \() -> Expect.equal "1.61 KiB" (formatBase2 1657)
            , test "227.61 MiB" <| \() -> Expect.equal "227.61 MiB" (formatBase2 238674052)
            ]
        , describe "Base 2 edge cases"
            [ test "1 B" <| \() -> Expect.equal "1 B" (formatBase2 1)
            , test "1023 B" <| \() -> Expect.equal "1023 B" (formatBase2 1023)
            , test "1 KiB" <| \() -> Expect.equal "1 KiB" (formatBase2 1024)
            , test "1023.99 KiB" <| \() -> Expect.equal "1023.99 KiB" (formatBase2 1048575)
            , test "1 MiB" <| \() -> Expect.equal "1 MiB" (formatBase2 1048576)
            , test "1023.99 MiB" <| \() -> Expect.equal "1023.99 MiB" (formatBase2 1073741823)
            , test "1 GiB" <| \() -> Expect.equal "1 GiB" (formatBase2 1073741824)
            , test "1023.99 GiB" <| \() -> Expect.equal "1023.99 GiB" (formatBase2 1099511627775)
            , test "1 TiB" <| \() -> Expect.equal "1 TiB" (formatBase2 1099511627776)
            , test "1023.99 TiB" <| \() -> Expect.equal "1023.99 TiB" (formatBase2 1125899906842623)
            , test "1 PiB" <| \() -> Expect.equal "1 PiB" (formatBase2 1125899906842624)
            ]
        ]


tuple_based : Test
tuple_based =
    describe "Tuple-based cases"
        [ describe "Basic cases"
            [ test "3 B" <| \() -> Expect.equal ( "3", "B" ) (formatSplit 3)
            , test "100 B" <| \() -> Expect.equal ( "100", "B" ) (formatSplit 100)
            , test "543 B" <| \() -> Expect.equal ( "543", "B" ) (formatSplit 543)
            , test "1.33 kB" <| \() -> Expect.equal ( "1.33", "kB" ) (formatSplit 1337)
            , test "238.67 MB" <| \() -> Expect.equal ( "238.67", "MB" ) (formatSplit 238674052)
            ]
        , describe "Edge cases"
            [ test "0 B" <| \() -> Expect.equal ( "0", "B" ) (formatSplit 0)
            , test "1 B" <| \() -> Expect.equal ( "1", "B" ) (formatSplit 1)
            , test "999 B" <| \() -> Expect.equal ( "999", "B" ) (formatSplit 999)
            , test "1 kB" <| \() -> Expect.equal ( "1", "kB" ) (formatSplit 1000)
            , test "999.99 kB" <| \() -> Expect.equal ( "999.99", "kB" ) (formatSplit 999999)
            , test "1 MB" <| \() -> Expect.equal ( "1", "MB" ) (formatSplit 1000000)
            , test "999.99 MB" <| \() -> Expect.equal ( "999.99", "MB" ) (formatSplit 999999999)
            , test "1 GB" <| \() -> Expect.equal ( "1", "GB" ) (formatSplit 1000000000)
            , test "999.99 GB" <| \() -> Expect.equal ( "999.99", "GB" ) (formatSplit 999999999999)
            , test "1 TB" <| \() -> Expect.equal ( "1", "TB" ) (formatSplit 1000000000000)
            , test "999.99 TB" <| \() -> Expect.equal ( "999.99", "TB" ) (formatSplit 999999999999999)
            , test "1 PB" <| \() -> Expect.equal ( "1", "PB" ) (formatSplit 1000000000000000)

            -- TODO For very large file sizes, the implementation of
            -- roundToDecimalPlaces can introduce rounding errors that lead to
            -- different results. Therefore, for now, we test with
            -- 999999999999999000 instead of 999999999999999999.
            , test "999.99 PB" <| \() -> Expect.equal ( "999.99", "PB" ) (formatSplit 999999999999999000)
            , test "1 EB" <| \() -> Expect.equal ( "1", "EB" ) (formatSplit 1000000000000000000)

            -- Somewhere around this (9223350000000000000) elm-format
            -- overwrites the Int literal with garbage, possibly due to an
            -- integer overflow. In order to keep this file editable with
            -- elm-format enabled, we do not use larger Ints in our tests.
            , test "922.33 EB" <| \() -> Expect.equal ( "9.22", "EB" ) (formatSplit 9223350000000000000)
            ]
        , describe "Basic negative cases"
            [ test "-3 B" <| \() -> Expect.equal ( "-3", "B" ) (formatSplit -3)
            , test "-100 B" <| \() -> Expect.equal ( "-100", "B" ) (formatSplit -100)
            , test "-543 B" <| \() -> Expect.equal ( "-543", "B" ) (formatSplit -543)
            , test "-1.33 kB" <| \() -> Expect.equal ( "-1.33", "kB" ) (formatSplit -1337)
            , test "-238.67 MB" <| \() -> Expect.equal ( "-238.67", "MB" ) (formatSplit -238674052)
            ]
        , describe "Negative edge cases"
            [ test "-1 B" <| \() -> Expect.equal ( "-1", "B" ) (formatSplit -1)
            , test "-999 B" <| \() -> Expect.equal ( "-999", "B" ) (formatSplit -999)
            , test "-1 kB" <| \() -> Expect.equal ( "-1", "kB" ) (formatSplit -1000)
            , test "-999.99 kB" <| \() -> Expect.equal ( "-999.99", "kB" ) (formatSplit -999999)
            , test "-1 MB" <| \() -> Expect.equal ( "-1", "MB" ) (formatSplit -1000000)
            , test "-999.99 MB" <| \() -> Expect.equal ( "-999.99", "MB" ) (formatSplit -999999999)
            , test "-1 GB" <| \() -> Expect.equal ( "-1", "GB" ) (formatSplit -1000000000)
            , test "-999.99 GB" <| \() -> Expect.equal ( "-999.99", "GB" ) (formatSplit -999999999999)
            , test "-1 TB" <| \() -> Expect.equal ( "-1", "TB" ) (formatSplit -1000000000000)
            , test "-999.99 TB" <| \() -> Expect.equal ( "-999.99", "TB" ) (formatSplit -999999999999999)
            , test "-1 PB" <| \() -> Expect.equal ( "-1", "PB" ) (formatSplit -1000000000000000)
            , test "-999.99 PB" <| \() -> Expect.equal ( "-999.99", "PB" ) (formatSplit -999999999999999000)
            , test "-1 EB" <| \() -> Expect.equal ( "-1", "EB" ) (formatSplit -1000000000000000000)
            , test "-922.33 EB" <| \() -> Expect.equal ( "-9.22", "EB" ) (formatSplit -9223350000000000000)
            ]
        , describe "Fuzzed cases"
            [ fuzz (intRange 0 999) "Positive byte range" <|
                \num ->
                    Expect.true "ends with \" B\""
                        (num |> formatSplit |> Tuple.second |> (==) "B")
            , fuzz (intRange 0 999) "Positive byte range sign" <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-" |> not)
            , fuzz (intRange -999 -1) "Negative byte range" <|
                \num ->
                    Expect.true "ends with \" B\""
                        (num |> formatSplit |> Tuple.second |> (==) "B")
            , fuzz (intRange -999 -1) "Negative byte range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-")
            , fuzz (intRange 1000 999999) "Positive kB range" <|
                \num ->
                    Expect.true "ends with \" kB\""
                        (num |> formatSplit |> Tuple.second |> (==) "kB")
            , fuzz (intRange 1000 999999) "Positive kB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-" |> not)
            , fuzz (intRange -999999 -1000) "Negative kB range" <|
                \num ->
                    Expect.true "ends with \" kB\""
                        (num |> formatSplit |> Tuple.second |> (==) "kB")
            , fuzz (intRange -999999 -1000) "Negative kB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-")
            , fuzz (intRange 1000000 999999999) "Positive MB range" <|
                \num ->
                    Expect.true "ends with \" MB\""
                        (num |> formatSplit |> Tuple.second |> (==) "MB")
            , fuzz (intRange 1000000 999999999) "Positive MB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-" |> not)
            , fuzz (intRange -999999999 -1000000) "Negative MB range" <|
                \num ->
                    Expect.true "ends with \" MB\""
                        (num |> formatSplit |> Tuple.second |> (==) "MB")
            , fuzz (intRange -999999999 -1000000) "Negative MB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-")
            , fuzz (intRange 1000000000 999999999999) "Positive GB range" <|
                \num ->
                    Expect.true "ends with \" GB\""
                        (num |> formatSplit |> Tuple.second |> (==) "GB")
            , fuzz (intRange 1000000000 999999999999) "Positive GB range sign " <|
                \num ->
                    Expect.true "has no leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-" |> not)
            , fuzz (intRange -999999999999 -1000000000) "Negative GB range" <|
                \num ->
                    Expect.true "ends with \" GB\""
                        (num |> formatSplit |> Tuple.second |> (==) "GB")
            , fuzz (intRange -999999999999 -1000000000) "Negative GB range sign" <|
                \num ->
                    Expect.true "has a leading minus"
                        (num |> formatSplit |> Tuple.first |> String.startsWith "-")
            ]
        , describe "With settings (decimalPlaces)"
            [ test "zero decimal places" <|
                \() ->
                    Expect.equal
                        ( "238", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = 0 } 238674052)
            , test "one decimal place" <|
                \() ->
                    Expect.equal
                        ( "238.6", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = 1 } 238674052)
            , test "three decimal places" <|
                \() ->
                    Expect.equal
                        ( "238.674", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = 3 } 238674052)
            , test "four decimal places with trailing zero" <|
                \() ->
                    Expect.equal
                        ( "238.674", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = 4 } 238674052)
            , test "238.67405 MB" <|
                \() ->
                    Expect.equal
                        ( "238.67405", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = 5 } 238674052)
            , test "use negative decimal places to round 1" <|
                \() ->
                    Expect.equal
                        ( "230", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = -1 } 238674052)
            , test "use negative decimal places to round 2" <|
                \() ->
                    Expect.equal
                        ( "200", "MB" )
                        (formatWithSplit { defaultSettings | decimalPlaces = -2 } 238674052)
            ]
        , describe "With settings (decimalSeparator)"
            [ test "use comma as decimal separator" <|
                \() ->
                    Expect.equal
                        ( "238,67", "MB" )
                        (formatWithSplit { defaultSettings | decimalSeparator = "," } 238674052)
            ]
        , describe "Base 2 basic cases"
            [ test "3 B" <| \() -> Expect.equal ( "3", "B" ) (formatBase2Split 3)
            , test "100 B" <| \() -> Expect.equal ( "100", "B" ) (formatBase2Split 100)
            , test "543 B" <| \() -> Expect.equal ( "543", "B" ) (formatBase2Split 543)
            , test "1.61 KiB" <| \() -> Expect.equal ( "1.61", "KiB" ) (formatBase2Split 1657)
            , test "227.61 MiB" <| \() -> Expect.equal ( "227.61", "MiB" ) (formatBase2Split 238674052)
            ]
        , describe "Base 2 edge cases"
            [ test "1 B" <| \() -> Expect.equal ( "1", "B" ) (formatBase2Split 1)
            , test "1023 B" <| \() -> Expect.equal ( "1023", "B" ) (formatBase2Split 1023)
            , test "1 KiB" <| \() -> Expect.equal ( "1", "KiB" ) (formatBase2Split 1024)
            , test "1023.99 KiB" <| \() -> Expect.equal ( "1023.99", "KiB" ) (formatBase2Split 1048575)
            , test "1 MiB" <| \() -> Expect.equal ( "1", "MiB" ) (formatBase2Split 1048576)
            , test "1023.99 MiB" <| \() -> Expect.equal ( "1023.99", "MiB" ) (formatBase2Split 1073741823)
            , test "1 GiB" <| \() -> Expect.equal ( "1", "GiB" ) (formatBase2Split 1073741824)
            , test "1023.99 GiB" <| \() -> Expect.equal ( "1023.99", "GiB" ) (formatBase2Split 1099511627775)
            , test "1 TiB" <| \() -> Expect.equal ( "1", "TiB" ) (formatBase2Split 1099511627776)
            , test "1023.99 TiB" <| \() -> Expect.equal ( "1023.99", "TiB" ) (formatBase2Split 1125899906842623)
            , test "1 PiB" <| \() -> Expect.equal ( "1", "PiB" ) (formatBase2Split 1125899906842624)
            ]
        ]
