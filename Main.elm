module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


-- import LegendaUno
-- MODEL


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



-- decodeLegenda LegendaUno.json


initialModel : Model
initialModel =
    { nome = "Legenda livello 1(uno)"
    , liv_due = "liv_due"
    , legenda = []
    , errorMessage = Nothing
    }


type alias Model =
    { nome : String
    , liv_due : String
    , legenda : List ItemLegenda
    , errorMessage : Maybe String
    }


type alias ItemLegenda =
    { liv_2 : String
    , liv_2_desc : String
    , rgb : String
    }



-- ACTION, UPDATE


type Msg
    = DoNothing
    | HandleLegendaResponse (Result Http.Error (List ItemLegenda))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        HandleLegendaResponse result ->
            case result of
                Ok results ->
                    ( { model | legenda = results, errorMessage = Nothing }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadPayload errorMessage _ ->
                            ( { model | errorMessage = Just errorMessage }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )



-- VIEW


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


px : Int -> String
px number =
    toString number ++ "px"


viewLegenda : Model -> Html msg
viewLegenda model =
    ul [ style [ "list-style-type" => "none" ] ]
        (List.map
            (\item ->
                div
                    [ style
                        [ "min-height" => "80px"
                        , "width" => "1000px"
                        , "color" => "black"
                        , "font-size" => "15px"
                        ]
                    , class "row"
                    ]
                    [ li
                        []
                        [ div []
                            [ div
                                [ style
                                    [ "background-color" => item.rgb
                                    , "color" => "black"
                                    , "width" => "50px"
                                    , "padding" => "2px"
                                    , "border-style" => "solid"
                                    , "border-width" => "0.1px"
                                    ]
                                , class "col-md-4"
                                ]
                                [ text item.liv_2 ]
                            ]
                        , div [ class "col-md-8" ] [ text item.liv_2_desc ]
                        ]
                    ]
            )
            model.legenda
        )


view : Model -> Html msg
view model =
    div [] [ text <| Maybe.withDefault "OK" model.errorMessage, viewLegenda model ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, richiestaWebLegenda initialModel.liv_due )
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        }


richiestaWebLegenda : String -> Cmd Msg
richiestaWebLegenda query =
    let
        url =
            "http://192.168.18.41:4000/api/"
                ++ query
    in
    Http.get url legendaDecoder
        |> Http.send HandleLegendaResponse
