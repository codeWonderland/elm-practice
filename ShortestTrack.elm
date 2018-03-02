module ShortestTrack exposing (..)

type alias Track = { title : String, duration : Int }

tracks =
  [ Track "Bakal" 524
  , Track "Violets for Your Furs" 378
  , Track "Time Was" 451
  ]

shortestTrack : List Track -> Maybe Track
shortestTrack trackList =
  let
    sortedList = List.sortBy .duration trackList
  in
    List.head sortedList