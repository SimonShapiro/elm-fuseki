module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, span, ul, li)
import Html.Attributes exposing (placeholder, value, class, rows, cols, wrap)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Http
import Html exposing (textarea)
import Html.Events exposing (on)
-- import Json.Decode exposing (Decoder, field, string, map3, list)

type Model 
    = Initialising
    | Pinging
    | Querying Sparql

type Msg 
    = NoOp
    | PingServer
    | Pinged (Result Http.Error ())
    | ChangeQuery Sparql
    | SubmitQuery Sparql
    | GotSparqlResponse (Result Http.Error SparqlResult)

type alias Server = String

type alias Sparql = String

type alias SparqlResult = String

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
                Ok _ -> 
                    Debug.log "Response OK"
                    (Querying "", Cmd.none)
                Err e -> 
                    Debug.log "Response ERROR"
                    (Initialising, Cmd.none)

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
                    , body = Http.stringBody "text" ""
                    , expect = Http.expectString GotSparqlResponse
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
-- Main

main: Program () Model Msg
main =
  Browser.element
    { init = initialModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }