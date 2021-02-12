module PlaygroundQuery exposing (..)

import Sparql exposing (..)
import Dict exposing (..)
import Regex exposing (..)
import String.Extra exposing (..)

type PlaygroundQuery 
    = SparqlQuery SparqlQuery
    | PlaygroundCommand PlaygroundCommand

type PlaygroundCommand
    = Graphs SparqlQuery
    | Ontology SparqlQuery
    | Size SparqlQuery

type alias CommandDict = Dict String SparqlQuery

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
        cmdGraphsRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^!"
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
        else if Regex.contains cmdGraphsRe query then query |> String.Extra.clean |> lookupCommand shriekCommands 
        else Unrecognised query

lookupCommand: CommandDict -> String -> SparqlQuery
lookupCommand dict cmd =
    Debug.log cmd
    Dict.get cmd dict |> Maybe.withDefault (Unrecognised cmd)


shriekCommands: CommandDict
shriekCommands =    [ ("!graphs", Select "select distinct ?graph (count(*) as ?count) {graph ?graph {?s ?p ?o}} group by ?graph")
                    , ("!ontology", Select """construct {?domain <hasProperty> ?predicate} {
                                    ?s ?predicate ?o.
                                    ?s a ?domain .
                                    } 
                                        """)
                    , ("!size", Select "select (count(*) as ?count) {?s ?p ?o}")
                    , ("!types", Select "select distinct ?type {?s a ?type} order by ?type")
                    , ("!predicates", Select "select distinct ?predicate {?s ?predicate ?o} order by ?predicate")
                    ] |> Dict.fromList



