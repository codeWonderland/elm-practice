module GalacticEmpire exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Http exposing (..)
import Random exposing (..)

main =
  program
  { init = init
  , update = update
  , view = view
  , subscriptions = \ _ -> Sub.none
  }

type alias Model = { id : Int, name : String }

init : (Model, Cmd Msg)
init = ({id = 1, name = ""}, Random.generate RandomId (Random.int 1 87))

type Msg = Next | RandomId Int | NewPerson (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  Next -> (model, getStarWarsData model.id)
  RandomId randId -> ({model | id = randId}, Cmd.none)
  NewPerson (Ok str) -> (model, Random.generate RandomId (Random.int 1 87))
  NewPerson (Err _)-> (model, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.name]
    , button [ onClick Next ] [ text "Next Entity" ]
    ]

getStarWarsData num =
  let url = "https://swapi.co/api/people/" ++ (toString num)
  in Http.send NewPerson (Http.get url decodeName)

decodeName : Decode.Decoder String
decodeName = Decode.field "name" Decode.string