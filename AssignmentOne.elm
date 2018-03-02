module AssignmentOne exposing (decToRoman)

--Got Some Inspiration from https://github.com/infertux/roman-numeral/blob/master/src/RomanNumeral.elm
decToRoman : Int -> Maybe String
decToRoman decimal =
    let
        digits =
            toString decimal
                            |> String.toList
                            |> List.reverse
                            |> List.map String.fromChar
                            |> List.map (\char -> String.toInt char |> Result.withDefault 0)

        parts =
            List.reverse <|
                List.indexedMap (\exp -> \digit -> digit * 10 ^ exp) digits
    in
        if decimal > 0 && decimal < 4000 then
            Just <| String.concat <| List.map reduce parts
        else
            Nothing

reduce : Int -> String
reduce decimal =
    let
        magnitude = toString decimal |> String.toList |> List.length |> toString

        head = toString decimal |> String.toList |> List.head

    in
        case magnitude of
            "1" ->
                case head of
                    Just '1' -> "I"
                    Just '2' -> "II"
                    Just '3' -> "III"
                    Just '4' -> "IV"
                    Just '5' -> "V"
                    Just '6' -> "VI"
                    Just '7' -> "VII"
                    Just '8' -> "VIII"
                    Just '9' -> "IX"
                    _ -> "Head Issue"
            "2" ->
                case head of
                    Just '1' -> "X"
                    Just '2' -> "XX"
                    Just '3' -> "XXX"
                    Just '4' -> "XL"
                    Just '5' -> "L"
                    Just '6' -> "LX"
                    Just '7' -> "LXX"
                    Just '8' -> "LXXX"
                    Just '9' -> "XC"
                    _ -> "Head Issue"
            "3" ->
                case head of
                    Just '1' -> "C"
                    Just '2' -> "CC"
                    Just '3' -> "CCC"
                    Just '4' -> "CD"
                    Just '5' -> "D"
                    Just '6' -> "DC"
                    Just '7' -> "DCC"
                    Just '8' -> "DCCC"
                    Just '9' -> "CM"
                    _ -> "Head Issue"
            "4" ->
                case head of
                    Just '1' -> "M"
                    Just '2' -> "MM"
                    Just '3' -> "MMM"
                    _ -> "Head Issue"

            _ -> "Unexpected Magnitude"
