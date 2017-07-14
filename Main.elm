module Main exposing (..)

import Html exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import LegendaUno


-- MODEL

test : List ItemLegenda
test =
  [ { liv_2 = "a", liv_2_desc = "prova", rgb = "#ff0000" }
  , { liv_2 = "b", liv_2_desc = "prova", rgb = "#ff0000" } ]

my_error : List ItemLegenda
my_error =
  [ { liv_2 = "e1", liv_2_desc = "prova", rgb = "#ff0000" }
  , { liv_2 = "e2", liv_2_desc = "prova", rgb = "#ff0000" } ]



itemLegendaDecoder : Decoder ItemLegenda
itemLegendaDecoder =
    decode ItemLegenda
        |> required "liv_2" string
        |> required "liv_2_desc" string
        |> required "rgb" string


legendaDecoder : Decoder (List ItemLegenda)
legendaDecoder =
    decode identity
        |> required "data" (list itemLegendaDecoder)


decodeLegenda : String -> List ItemLegenda
decodeLegenda json =
    case decodeString legendaDecoder json of
        Ok itemsLegenda ->
            test

        _ ->
            my_error


model : Model
model =
    { nome = "Legenda livello 1(uno)"
    , legenda = decodeLegenda LegendaUno.json
    }


type alias Model =
    { nome : String
    , legenda : List ItemLegenda
    }


type alias ItemLegenda =
    { liv_2 : String
    , liv_2_desc : String
    , rgb : String
    }



-- ACTION, UPDATE


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    ul [] (List.map (\item -> li [] [ text item.liv_2 ]) model.legenda)


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
