module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, span, ul, li, b, p, hr)
import Html.Attributes exposing (placeholder, value, class, rows, cols, wrap)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Http
import Html exposing (textarea)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, Error, errorToString, field, string, map, map2, map3, map4, map5, map6, map7, list, int, decodeString, at, andThen)

import File exposing (File)
import File.Select as Select
import Task

type Model 
    = Initialising Server
    | Pinging Server
    | Querying Server Sparql
    | DisplayingSelectResult Server Sparql (List (List SelectAtom))
    | DisplayingSelectError Server Sparql String
    | ApiError Http.Error

type Msg 
    = NoOp
    | ChangeServer Server
    | PingServer Server
    | Pinged Server (Result Http.Error ())
    | ChangeQuery Server Sparql
    | SubmitQuery Server Sparql
    | GotSparqlResponse (Result Http.Error KGResponse)
    | FileRequested 
    | FileSelected File
    | FileLoaded Sparql

type alias Server = String

type alias Sparql = String

type alias KGResponse =  -- a copy of the query is available in the api
    { server: Server
    , status: Int
    , message: String
    , queryType: String
    , query: Sparql
    , vars: (List String)
    , result: (List (List SelectAtom))
    }

type alias SelectResult = String

type alias SelectFailure = String

type alias SelectAtom =
    { key: String
    , value: String
    }

selectAtomDecoder: Decoder SelectAtom
selectAtomDecoder = 
    map2 SelectAtom
        (field "key" string)
        (field "value" string )

server: Server
server = "http://localhost:port"

initialModel: flags -> (Model, (Cmd Msg))
initialModel _ = (Initialising server, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (Initialising "", Cmd.none)
        ChangeServer newServer -> (Initialising newServer, Cmd.none)
        PingServer newServer -> (Pinging newServer, pingServer newServer)
        Pinged newServer result ->
            case result of
                Ok _ -> 
                    Debug.log "Pinged OK"
                    (Querying newServer "", Cmd.none)
                Err e -> 
                    Debug.log "Pinged ERROR"
                    (Initialising "", Cmd.none)
        ChangeQuery newServer newQuery -> 
            Debug.log ("Query "++newQuery)
            (Querying newServer newQuery, Cmd.none)
        SubmitQuery newServer query -> 
            Debug.log ("Submitting Query "++query)
            (Querying newServer query, submitQuery newServer query)
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    case okData.status of
                        200 ->
                            (DisplayingSelectResult okData.server okData.query okData.result, Cmd.none)
                        _ ->
                            (DisplayingSelectError okData.server okData.query okData.message, Cmd.none)
                Err e -> 
                    Debug.log "Response ERROR"
                    (ApiError e, Cmd.none)
        FileRequested  ->
                ( model
                , Select.file ["text"] FileSelected
                )  --          (Querying newServer "", Cmd.none)
        FileSelected file ->        
                ( model
                , Task.perform FileLoaded (File.toString file)
                )
        FileLoaded content ->
                Debug.log content
                ( model, Cmd.none)
                
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

pingServer newServer = 
  --  let
        Debug.log ("Pinging"++newServer)

  --  in
        Http.request
                    { method = "HEAD"
                    , headers = []
                    , url = newServer++"/hello"
                    , body = Http.stringBody "text" ""
                    , expect = Http.expectWhatever (Pinged newServer)
                    , timeout = Nothing
                    , tracker = Nothing
                    }

submitQuery: Server -> Sparql -> (Cmd  Msg)
submitQuery newServer query = 
        Http.request
                    { method = "POST"
                    , headers = [Http.header "Content-Type" "application/sparql-request"]
                    , url = newServer++"/sparql"
                    , body = Http.stringBody "text" query
                    , expect = Http.expectJson GotSparqlResponse mainDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

-- View

queryInput: Server -> Sparql -> Html Msg
queryInput newServer query =
            div [] 
                [ textarea 
                    [ cols 120
                    , rows 15
                    , wrap "soft"
                    , placeholder "Sparql Query"
                    , onInput (ChangeQuery newServer)
                    , value query
                    ][]
                , button [onClick (SubmitQuery newServer query)][text "Submit"]
                ]

uploadQueryFromFile:  Html Msg
uploadQueryFromFile = 
    div []
    [ button [onClick FileRequested][text "Load"]]

view: Model -> Html Msg
view model = 
    case model of
        Initialising newServer ->
            div [] 
                [ h1 [][text "hello world"]
                , div [] 
                    [ input [placeholder "Server", onInput ChangeServer, value newServer][]
                    , button [onClick (PingServer newServer)][text "Connect"]
                    ]
                ]
        Pinging newServer-> 
            div [][text <| "Pinging"++newServer]
        Querying newServer query -> div []
                [ uploadQueryFromFile
                , queryInput newServer query
                ]
        ApiError error -> 
            case error of
                Http.BadBody err ->
                    div []
                        [ h1 [][text "I can't interpret the response"]
                        , div [][text err]
--                        , button [onClick <| ChangeQuery ""][text "<-Query"]  -- need onldQuery instead of ""
                        ]
                _ ->
                    div [][
                        h1 [][text "Oops - something went wrong! :-("]
                    ]
        DisplayingSelectError newServer query message ->
            div []                
                [ uploadQueryFromFile
                , queryInput newServer query
                , div [][text message]
                ]
        DisplayingSelectResult newServer query result ->
            div []                
                [ uploadQueryFromFile
                , queryInput newServer query
                , div []
                    (List.map (
                        \row ->
                        div [][
                            div []
                            (List.map (
                                \var -> 
                                    div []
                                        [ b [][text var.key]
                                        , text ": "
                                        , text var.value
                                        ]
                            ) row)
                            , hr [][]]
                    ) result)
--                , button [onClick <| ChangeQuery ""][text "<-Query"]  -- need onldQuery instead of ""
            ]
-- Decoders

mainDecoder: Decoder KGResponse
mainDecoder =
     map7 KGResponse
        (field "server" string)
        (field "status" int)
        (field "reason" string)
        (field "queryType" string)
        (field "query" string)
        (field "vars" (list string))
        (field "result" (list (list selectAtomDecoder)))    
-- Main

main: Program () Model Msg
main =
  Browser.element
    { init = initialModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }