module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, span, ul, li)
import Html.Attributes exposing (placeholder, value, class, rows, cols, wrap)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Http
import Html exposing (textarea)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, Error, field, string, map, map3, map4, map5, list, int, decodeString, at, andThen)

type Model 
    = Initialising
    | Pinging
    | Querying Sparql
    | DisplayingSelectResult (List String)
    | ApiError Http.Error

type Msg 
    = NoOp
    | PingServer
    | Pinged (Result Http.Error ())
    | ChangeQuery Sparql
    | SubmitQuery Sparql
    | GotSparqlResponse (Result Http.Error KGResponse)

type alias Server = String

type alias Sparql = String

type alias SparqlResult = String

type alias KGResponse =  -- a copy of the query is available in the api
    { status: Int
    , message: String
    , queryType: String
    , result: String
    }

server: Server
server = "http://localhost:5000"

endpoint: Server
endpoint = server ++ "/sparql"

initialModel: flags -> (Model, (Cmd Msg))
initialModel _ = (Initialising, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (Initialising, Cmd.none)
        PingServer -> (Pinging, pingServer)
        Pinged result ->
            case result of
                Ok _ -> 
                    Debug.log "Pinged OK"
                    (Querying "", Cmd.none)
                Err e -> 
                    Debug.log "Pinged ERROR"
                    (Initialising, Cmd.none)
        ChangeQuery newQuery -> 
            Debug.log ("Query "++newQuery)
            (Querying newQuery, Cmd.none)
        SubmitQuery query -> 
            Debug.log ("Submitting Query "++query)
            (Querying query, submitQuery query)
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    let
                        data = okData.result
                        decoder = at [ "head", "vars" ] (list string)
                        varsDecoded = decodeString decoder data
                    in
                        case varsDecoded of
                            Ok vars -> decodeBindings data vars
                            Err e -> 
                                Debug.log "Vars ERROR"
                                (ApiError (Http.BadBody "Failed to decode vars"), Cmd.none)

                Err e -> 
                    Debug.log "Response ERROR"
                    (ApiError e, Cmd.none)

decodeBindings: String -> (List String) -> (Model, (Cmd Msg))
decodeBindings result vars =
                       Debug.log ("Response OK"++result)
                       (DisplayingSelectResult vars, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

pingServer = 
  --  let
        Debug.log ("Pinging"++server)

  --  in
        Http.request
                    { method = "HEAD"
                    , headers = []
                    , url = server++"/hello"
                    , body = Http.stringBody "text" ""
                    , expect = Http.expectWhatever Pinged
                    , timeout = Nothing
                    , tracker = Nothing
                    }

submitQuery: Sparql -> (Cmd  Msg)
submitQuery query = 
        Http.request
                    { method = "POST"
                    , headers = [Http.header "Content-Type" "application/sparql-request"]
                    , url = server++"/sparql"
                    , body = Http.stringBody "text" query
                    , expect = Http.expectJson GotSparqlResponse mainDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

view: Model -> Html Msg
view model = 
    case model of
        Initialising ->
            div [] 
                [ h1 [][text "hello world"]
                , div [] 
                    [ button [onClick PingServer][text "Connect"]
                    ]
                ]
        Pinging -> 
            div [][text <| "Pinging"++server]
        Querying query ->
            div [] 
                [ textarea 
                    [ cols 120
                    , rows 30
                    , wrap "soft"
                    , placeholder "Sparql Query"
                    , onInput ChangeQuery
                    ][]
                , button [onClick (SubmitQuery query)][text "Submit"]
                ]
        ApiError error -> 
            case error of
                Http.BadBody err ->
                    div []
                        [ h1 [][text "I can't interpret the response"]
                        , div [][text err]
                        ]
                _ ->
                    h1 [][text "Oops - something went wrong! :-("]
        DisplayingSelectResult result ->
            varsView result

varsView: (List String) -> Html Msg
varsView vars =
    div []
        (List.map (\v ->
            div [] [text v]) vars)
-- Decoders

mainDecoder: Decoder KGResponse
mainDecoder =
     map4 KGResponse
        (field "status" int)
        (field "reason" string)
        (field "queryType" string)
        (field "result" string)
    
getVars: String -> Result Json.Decode.Error String
getVars inString =
    let 
        varsString =
            inString 
            |> (decodeString (at ["head", "vars"] string))
    in
        case varsString of
            Ok vars ->
                Ok vars 
            Err e -> Err e
-- Main

main: Program () Model Msg
main =
  Browser.element
    { init = initialModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }