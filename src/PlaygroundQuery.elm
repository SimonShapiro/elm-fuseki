module PlaygroundQuery exposing (..)

import Sparql exposing (..)
import Dict exposing (..)
import Regex exposing (..)

type PlaygroundQuery 
    = SparqlQuery SparqlQuery
    | PlaygroundCommand PlaygroundCommand

type PlaygroundCommand
    = Graphs SparqlQuery
    | Ontology SparqlQuery
    | Size SparqlQuery

toString: PlaygroundQuery -> String
toString playQuery =
    case playQuery of
        SparqlQuery q -> Sparql.toString q
        PlaygroundCommand cmd -> 
            case cmd of
                Graphs q -> Sparql.toString q
                Ontology q -> Sparql.toString q
                Size q -> Sparql.toString q

establishQueryType: String -> SparqlQuery
establishQueryType query = 
    let
        selectRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^select"
        askRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^ask"
        constructRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^construct"
        describeRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^describe"
        insertRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^insert"
        deleteRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^delete"
        loadRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^load"
        dropRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^drop"
        createRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^create"
        clearRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^clear"
    in
        if Regex.contains selectRe query then Select query
        else if Regex.contains askRe query then Ask query
        else if Regex.contains constructRe query then Construct query
        else if Regex.contains describeRe query then Describe query
        else if Regex.contains insertRe query then Insert query
        else if Regex.contains deleteRe query then Delete query
        else if Regex.contains loadRe query then Load query
        else if Regex.contains dropRe query then Drop query
        else if Regex.contains clearRe query then Clear query
        else if Regex.contains createRe query then Create query
        else Unrecognised query


shriekCommands: Dict String PlaygroundCommand
shriekCommands =    [ ("!graphs", Graphs <| Select "select distinct ?graph (count(*) as ?count) {graph ?g {?S ?p ?o} group by ?graph}")
                    , ("!ontology", Ontology <| Select """select distinct ?domain ?predicate {
                                    ?s ?predicate ?o.
                                    ?s a ?domain .
                                    } order by ?domain ?predicate
                                        """)
                    , ("!size", Size <| Select "(count(*) as ?count) {?s ?p ?o}")
                    ] |> Dict.fromList



