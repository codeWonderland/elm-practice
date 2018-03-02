-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/web_sockets.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import WebSocket
import Json.Decode exposing (..)
import Round exposing (..)

main =
  Html.program
    { subscriptions = subscriptions
    , init = init
    , view = view
    , update = update
    }


gdaxServer : String
gdaxServer =
  "wss://ws-feed.gdax.com"


gdaxArgs : String
gdaxArgs =
  "{ \"type\": \"subscribe\", \"product_ids\": [\"BTC-USD\"], \"channels\": [ {\"name\": \"ticker\", \"product_ids\": [\"BTC-USD\"]}]}"


unsubArgs : String 
unsubArgs =
  "{ \"type\": \"unsubscribe\", \"product_ids\": [\"BTC-USD\"], \"channels\": [ {\"name\": \"ticker\", \"product_ids\": [\"BTC-USD\"]}]}"

-- MODEL


type alias Model =
  { messages : List String }


init : (Model, Cmd Msg)
init =
  (Model [], WebSocket.send gdaxServer gdaxArgs)



-- UPDATE


type Msg
  = Start
  | Stop
  | NewMessage String


update : Msg -> Model -> (Model, Cmd Msg)
update msg {messages} =
  case msg of
    Start -> 
        (Model [], WebSocket.send gdaxServer gdaxArgs)

    Stop ->
        (Model [], WebSocket.send gdaxServer unsubArgs)

    NewMessage str ->
      (Model (str :: messages), Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen gdaxServer NewMessage



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ Html.button [onClick Start] [Html.text "Start"]
    , Html.button [onClick Stop] [Html.text "Stop"]
    , div [] (List.map viewMessage (List.reverse model.messages))
    ]


viewMessage : String -> Html msg
viewMessage msg =
  div [] [ text (parseJson(msg)) ]



-- Utility Functions


parseJson : String -> String
parseJson msg =
    let 
        datum =
            Round.roundCom 2 <| 
            Result.withDefault 0 <| 
            String.toFloat <| 
            String.filter isValid <|
            toString <|
            decodeString (at ["price"] string) msg
       
    in
        case datum of
            "0.00" ->
                "Live Bitcoin Cost : "
            _ ->
                datum

isValid : Char -> Bool
isValid char = char /= 'O' && char /= 'k' && char /= ' ' && char /= '"'

