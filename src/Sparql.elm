module Sparql exposing (..)

import Regex exposing (..)
import Http exposing (..)
import Task exposing (..)
import Html exposing (pre)
import Dict exposing (..)

type alias Server = String
type alias ContentType = String
type alias AcceptHeader = String
type alias QueryType = String

type SparqlQuery 
    = Select String
    | Ask String
    | Construct String
    | Describe String
    | Insert String
    | Delete String
    | Load String
    | Drop String
    | Clear String
    | Create String
    | Unrecognised String

toString: SparqlQuery -> String
toString sparql = 
  --  Maybe String ???
    case sparql of
       Unrecognised s -> s 
       Select s -> s
       Ask s -> s
       Construct s -> s
       Describe s -> s
       Insert s -> s
       Delete s -> s
       Load s -> s
       Drop s -> s
       Clear s -> s
       Create s -> s

prepareHttpRequest: Server -> ContentType -> AcceptHeader -> QueryType -> String -> (Http.Expect msg) -> (Cmd msg)
prepareHttpRequest newServer contentType header qtype query expect =
    let
        _ = Debug.log "Posting to server" query
    in
        Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" contentType   -- generalise for update
                            , Http.header "Accept" header
                            , Http.header "x-Qtype" qtype
                            ]
                , url = newServer++"/sparql"
                , body = Http.stringBody "text" query
                , expect = expect -- Http.expectJson  GotSparqlResponse mainDecoder -- this could be parameterised
                , timeout = Nothing
                , tracker = Nothing
                }

submitQuery: Server -> SparqlQuery -> (Http.Expect msg) -> (Cmd  msg)
submitQuery newServer sparql expect = 
    case sparql of
        Select query -> prepareHttpRequest newServer "application/sparql-request" "application/json" "select" query expect
        Ask query -> prepareHttpRequest newServer "application/sparql-request" "application/json" "ask" query expect
        Construct query -> prepareHttpRequest newServer "application/sparql-request" "application/ld+json" "construct" query expect
        Describe query -> prepareHttpRequest newServer"application/sparql-request"  "application/ld+json" "describe" query expect
        Insert query -> prepareHttpRequest newServer  "application/sparql-update" "application/text" "insert" query expect
        Delete query -> prepareHttpRequest newServer  "application/sparql-update" "application/text" "delete" query expect
        Load query -> prepareHttpRequest newServer  "application/sparql-update" "application/text" "load" query expect
        Drop query -> prepareHttpRequest newServer "application/sparql-update" "application/text" "drop" query expect
        Create query -> prepareHttpRequest newServer "application/sparql-update" "application/text" "create" query expect
        Clear query -> prepareHttpRequest newServer "application/sparql-update" "application/text" "clear" query expect
        Unrecognised query -> Cmd.none
