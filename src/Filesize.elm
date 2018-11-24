module Filesize exposing
    ( format, formatBase2, formatWith, defaultSettings, Settings, Units(..)
    , formatBase2Split, formatSplit, formatWithSplit
    )

{-| This library converts a file size in bytes into a human readable string.

Examples:

    format 1234 == "1.23 kB"

    format 238674052 == "238.67 MB"

    format 543 == "543 B"

You can either use decimal units (also known as base 10 units, these are the
default) or binary (also called base 2 or IEC units).

Supported decimal units:

  - 1 byte (B)
  - 1 kilobyte (kB) = 1000 bytes
  - 1 megabyte (MB) = 1000 kilobytes
  - 1 gigabyte (GB) = 1000 megabytes
  - 1 terabyte (TB) = 1000 gigabytes
  - 1 petabyte (PB) = 1000 terabytes
  - 1 exabyte (EB) = 1000 petabyte

Larger decimal units (zettabyte (ZB), yottabyte (YB), ...) are not supported.

Supported binary/IEC units:

  - 1 byte (B)
  - 1 kibibyte (KiB) = 1024 bytes
  - 1 mebibyte (MiB) = 1024 kibibytes
  - 1 gibibyte (GiB) = 1024 mebibytes
  - 1 tebibyte (TiB) = 1024 gibibytes
  - 1 pebibyte (PiB) = 1024 tebibyte

Larger binary units (exbibyte (EiB), zebibyte (ZiB), yobibytej (YiB), ...)) are
not supported.

For decimal/base 10 units, the number of bytes is divided by 10^3 when going to
the next larger unit. For binary/base 2 units, the number of bytes is divided by
2^10 (1024) each time. (For binary units also see
<https://en.wikipedia.org/wiki/Kibibyte>.)


## Usage

@docs format, formatBase2, formatWith, defaultSettings, Settings, Units

-}

import Regex exposing (Regex, replaceAtMost)
import Round


{-| The two possible unit types, either decimal/base 10 (kb, MB, GB, ...) or
binary/IEC/base 2 (KiB, MiB, GiB, ...), see above.
-}
type Units
    = Base10
    | Base2


{-| Use a settings record together with `formatWith` to customize the formatting
process. The available options are:

  - `units`: use either decimal or binary/IEC units (the default is to use decimal
    units),
  - `decimalPlaces`: the number of decimal places (digits after the decimal
    separator, the default is 2) and
  - `decimalSeparator`: the decimal separator to use (default ".").

-}
type alias Settings =
    { units : Units
    , decimalPlaces : Int
    , decimalSeparator : String
    }


{-| The default settings. When using `formatWith`, it is recommended to obtain
a settings record with this function and modify the settings to your liking.
-}
defaultSettings : Settings
defaultSettings =
    { units = Base10
    , decimalPlaces = 2
    , decimalSeparator = "."
    }


type alias UnitDefinition =
    { minimumSize : Int
    , abbreviation : String
    }


type alias UnitDefinitionList =
    List UnitDefinition


base10UnitList : UnitDefinitionList
base10UnitList =
    [ { minimumSize = 1, abbreviation = "B" }
    , { minimumSize = 1000, abbreviation = "kB" }
    , { minimumSize = 1000000, abbreviation = "MB" }
    , { minimumSize = 1000000000, abbreviation = "GB" }
    , { minimumSize = 1000000000000, abbreviation = "TB" }
    , { minimumSize = 1000000000000000, abbreviation = "PB" }
    , { minimumSize = 1000000000000000000, abbreviation = "EB" }

    -- , { miniumVersion = 1000000000000000000000, abbreviation = "ZB" }
    -- , { miniumVersion = 1000000000000000000000000, abbreviation = "YB" }
    -- , ...
    ]


base2UnitList : UnitDefinitionList
base2UnitList =
    [ { minimumSize = 1, abbreviation = "B" }
    , { minimumSize = 1024, abbreviation = "KiB" }
    , { minimumSize = 1048576, abbreviation = "MiB" }
    , { minimumSize = 1073741824, abbreviation = "GiB" }
    , { minimumSize = 1099511627776, abbreviation = "TiB" }
    , { minimumSize = 1125899906842624, abbreviation = "PiB" }

    -- , ...
    ]


getUnitDefinitionList : Units -> UnitDefinitionList
getUnitDefinitionList units =
    case units of
        Base10 ->
            base10UnitList

        Base2 ->
            base2UnitList


unknownUnit : UnitDefinition
unknownUnit =
    { minimumSize = 1, abbreviation = "?" }


decimalSeparatorRegex : Regex
decimalSeparatorRegex =
    "\\." |> Regex.fromString |> Maybe.withDefault Regex.never


removeTrailingZeroesRegex : Regex
removeTrailingZeroesRegex =
    "^(\\d+\\.[^0]*)(0+)$" |> Regex.fromString |> Maybe.withDefault Regex.never


{-| Formats the given file size with the default settings.

Convenience function for

    let
        ( size, unit ) =
            formatWithSplit settings num
    in
    size ++ " " ++ unit

-}
format : Int -> String
format num =
    let
        ( size, unit ) =
            formatWithSplit defaultSettings num
    in
    size ++ " " ++ unit


{-| Formats the given file size with the default settings, returning the number and units separately, in a tuple.
-}
formatSplit : Int -> ( String, String )
formatSplit =
    formatWithSplit defaultSettings


{-| Formats the given file size with the binary/base2/IEC unit.
-}
formatBase2 : Int -> String
formatBase2 =
    formatWith { defaultSettings | units = Base2 }


{-| Formats the given file size with the binary/base2/IEC unit, returning the number and units separately, in a tuple.
-}
formatBase2Split : Int -> ( String, String )
formatBase2Split =
    formatWithSplit { defaultSettings | units = Base2 }


{-| Formats the given file size with the given settings.
-}
formatWith : Settings -> Int -> String
formatWith settings num =
    let
        ( size, unit ) =
            formatWithSplit settings num
    in
    size ++ " " ++ unit


{-| Formats the given file size with the given settings, returning the number and units separately, in a tuple.
-}
formatWithSplit : Settings -> Int -> ( String, String )
formatWithSplit settings num =
    if num == 0 then
        ( "0", "B" )

    else
        let
            ( num2, negativePrefix ) =
                if num < 0 then
                    ( num |> negate, "-" )

                else
                    ( num, "" )

            unitDefinitionList =
                getUnitDefinitionList settings.units

            unitDefinition =
                unitDefinitionList
                    |> List.filter (\unitDef -> num2 >= unitDef.minimumSize)
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault unknownUnit

            formattedNumber =
                toFloat num2
                    / toFloat unitDefinition.minimumSize
                    |> roundToDecimalPlaces settings
        in
        ( negativePrefix ++ formattedNumber, unitDefinition.abbreviation )


roundToDecimalPlaces : Settings -> Float -> String
roundToDecimalPlaces settings num =
    let
        -- Actually, using Round.round instead of floor would be preferable but
        -- we never want to round from 999.999 to 1000 because then we would
        -- combine the number with the wrong unit (the proper unit has been
        -- calculated before rounding). Maybe we should switch rounding and unit
        -- selection to avoid this?
        rounded =
            Round.floor {- option -} settings.decimalPlaces num

        -- TODO all this removal of trailing zeroes, trailing dots and replacing
        -- of the decimal separator should be options that elm-round provides.
        -- https://github.com/myrho/elm-round/pull/2 makes the start, let's see
        -- how this goes.
        withoutTrailingZeroes =
            Regex.replaceAtMost 1
                removeTrailingZeroesRegex
                (\{ submatches } ->
                    submatches
                        |> List.take 1
                        |> List.map (Maybe.withDefault "")
                        |> String.join ""
                )
                rounded

        withoutTrailingDot =
            if String.endsWith "." withoutTrailingZeroes then
                String.dropRight 1 withoutTrailingZeroes

            else
                withoutTrailingZeroes
    in
    if settings.decimalSeparator == "." then
        withoutTrailingDot

    else
        Regex.replaceAtMost 1
            decimalSeparatorRegex
            (\_ -> settings.decimalSeparator)
            withoutTrailingDot
