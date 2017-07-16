module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import LegendaUno


-- MODEL


test : List ItemLegenda
test =
    [ { liv_2 = "a", liv_2_desc = "prova", rgb = "#ff0000" }
    , { liv_2 = "b", liv_2_desc = "prova", rgb = "#ff0000" }
    ]


my_error : List ItemLegenda
my_error =
    [ { liv_2 = "e1", liv_2_desc = "prova", rgb = "#ff0000" }
    , { liv_2 = "e2", liv_2_desc = "prova", rgb = "#ff0000" }
    ]


itemLegendaDecoder : Decoder ItemLegenda
itemLegendaDecoder =
    decode ItemLegenda
        |> Json.Decode.Pipeline.required "liv_2" string
        |> Json.Decode.Pipeline.required "liv_2_desc" string
        |> Json.Decode.Pipeline.required "rgb" string


legendaDecoder : Decoder (List ItemLegenda)
legendaDecoder =
    decode identity
        |> Json.Decode.Pipeline.required "data" (Json.Decode.list itemLegendaDecoder)


decodeLegenda : String -> List ItemLegenda
decodeLegenda json =
    case decodeString legendaDecoder json of
        Ok itemsLegenda ->
            itemsLegenda

        Err errore ->
            let
                _ =
                    Debug.log errore []
            in
            []


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


(=>) =
    (,)


px : Int -> String
px number =
    toString number ++ "px"


view : Model -> Html Msg
view model =
    ul []
        (List.map
            (\item ->
                div
                    [ style
                        [ "background-color" => item.rgb
                        , "width" => "auto"
                        , "height" => "20px"
                        , "border-radius" => "4px"
                        , "color" => "black"
                        , "display" => "flex"
                        ]
                    ]
                    [ li
                        []
                        [ text item.liv_2, text " - ", text item.liv_2_desc ]
                    ]
            )
            model.legenda
        )


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }
