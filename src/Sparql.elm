module Sparql exposing (..)

import Regex exposing (..)
import Http exposing (..)
import Task exposing (..)

type alias Server = String

type SparqlQuery 
    = Select String
    | Ask String
    | Construct String
    | Describe String
    | Unrecognised

toString: SparqlQuery -> String
toString sparql = 
    case sparql of
       Unrecognised -> ""
       Select s -> s
       Ask s -> s
       Construct s -> s
       Describe s -> s

establishQueryType: String -> SparqlQuery
establishQueryType query = 
    let
        selectRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^select"
        askRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^ask"
        constructRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^construct"
        describeRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^describe"
    in
        if Regex.contains selectRe query then Select query
        else if Regex.contains askRe query then Ask query
        else if Regex.contains constructRe query then Construct query
        else if Regex.contains describeRe query then Describe query
        else Unrecognised  

prepareHttpRequest: Server -> String -> String -> String -> (Http.Expect msg) -> (Cmd msg)
prepareHttpRequest newServer header qtype query expect =
    Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/sparql-request"  -- generalise for update
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
        Select query -> prepareHttpRequest newServer "application/json" "select" query expect
        Ask query -> prepareHttpRequest newServer "application/json" "ask" query expect
        Construct query -> prepareHttpRequest newServer "application/ld+json" "construct" query expect
        Describe query -> prepareHttpRequest newServer "application/ld+json" "describe" query expect
        Unrecognised -> Cmd.none
