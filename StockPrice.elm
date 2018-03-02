module StockPrice exposing (..)

type alias StockPrice = { price : Int, stock : String }

stocks : List StockPrice
stocks =
  [ StockPrice 1084 "GOOG"
  , StockPrice 175 "AAPL"
  , StockPrice 92 "MSFT"
  , StockPrice 176 "FB"
  , StockPrice 48 "INTC"
  , StockPrice 1489 "AMZN"
  ]

bestStock : List StockPrice -> Maybe StockPrice
bestStock stockList =
  let
      filteredList = List.filterMap below200 stocks
      sortedList = List.reverse <| List.sortBy .price filteredList
    in
      List.head sortedList


below200 : StockPrice -> Maybe StockPrice
below200 n =
  if n.price <= 200 then
    Just n

  else
    Nothing
