module PlaygroundQuery exposing (..)

import Sparql exposing (..)
import Dict exposing (..)

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
        _ -> "A play query"

shriekCommands: Dict String PlaygroundCommand
shriekCommands =    [ ("!graphs", Graphs <| Select "select distinct ?graph (count(*) as ?count) {graph ?g {?S ?p ?o} group by ?graph}")
                    , ("!ontology", Ontology <| Select """select distinct ?domain ?predicate {
                                    ?s ?predicate ?o.
                                    ?s a ?domain .
                                    } order by ?domain ?predicate
                                        """)
                    , ("!size", Size <| Select "(count(*) as ?count) {?s ?p ?o}")
                    ] |> Dict.fromList



