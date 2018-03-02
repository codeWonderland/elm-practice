module Score exposing (..)

import String exposing (..)

addLetter : String -> Int
addLetter s =
  let
    c = toUpper s
    total = 0
  in
    if contains c "AEIOULNRST" then total + 1
    else if contains c "DG" then total + 2
    else if contains c "BCMP" then total + 3
    else if contains c "FHVWY" then total + 4
    else if contains c "K" then total + 5
    else if contains c "JX" then total + 8
    else if contains c "QZ" then total + 10
    else total

totalWord : String -> Int
totalWord word =
  List.sum (List.map addLetter (split "" word))
