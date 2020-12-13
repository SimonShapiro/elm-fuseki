module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, span, ul, li, b, p, hr)
import Html.Attributes exposing (placeholder, value, class, rows, cols, wrap)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Http
import Html exposing (textarea)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, Error, errorToString, field, string, map, map2, map3, map4, map5, map6, list, int, decodeString, at, andThen)

type Model 
    = Initialising
    | Pinging
    | Querying Sparql
    | DisplayingSelectResult Sparql (List (List SelectAtom))
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

type alias KGResponse =  -- a copy of the query is available in the api
    { status: Int
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
server = "http://localhost:5000"

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
                    case okData.status of
                        200 ->
                            (DisplayingSelectResult okData.query okData.result, Cmd.none)
                        _ ->
                            (ApiError <| Http.BadBody okData.message, Cmd.none)
                Err e -> 
                    Debug.log "Response ERROR"
                    (ApiError e, Cmd.none)
       
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

-- View

queryInput: Sparql -> Html Msg
queryInput query =
            div [] 
                [ textarea 
                    [ cols 120
                    , rows 15
                    , wrap "soft"
                    , placeholder "Sparql Query"
                    , onInput ChangeQuery
                    , value query
                    ][]
                , button [onClick (SubmitQuery query)][text "Submit"]
                ]

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
            queryInput query
        ApiError error -> 
            case error of
                Http.BadBody err ->
                    div []
                        [ h1 [][text "I can't interpret the response"]
                        , div [][text err]
                        ]
                _ ->
                    h1 [][text "Oops - something went wrong! :-("]
        DisplayingSelectResult query result ->
            div [][
                queryInput query
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
     map6 KGResponse
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