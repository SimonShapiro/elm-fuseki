module RdfDict exposing (..) 

--(RdfDict, makeRdfDict, RdfNode, ServerVars, ServerForm, SelectAtom, SubjectMolecule, rdfNodeToMaybeString)

import Dict exposing(Dict)
import List.Extra exposing (uncons, groupWhile)
import Maybe.Extra exposing (combine, join)

type alias ServerForm a = (List (List a))

type alias ServerVars = (List String)

type alias TripleForm a = (List (a, a, a))

type alias ContractedForm a = (List (SubjectMolecule a)) 
type alias SubjectMolecule a = (a, (List (a, List a)))  -- s, List (pred, obj)

type alias SelectAtom =
    { key: String
    , value: String
    , aType: String
    , language: String
    , datatype: String
    }

type RdfNode 
    = Uri {value: String}
    | BlankNode {value: String}
    | LiteralOnlyValue {value: String}
    | LiteralValueAndDataType {value: String, dataType: String}
    | LiteralValueAndLanguageString {value: String, language: String}
    | Unknown

type alias RdfKey = String
type alias RdfDict = Dict RdfKey (SubjectMolecule RdfNode)  -- the main form of the graph -- consider replacing with elm-community/graph
makeTripleForm: ServerForm a -> Maybe (TripleForm  a)
makeTripleForm res = res 
-- ensure that serverForm consists of triples (not necessarily spo)
                    |> List.map (\r -> makeTriple r)
                    |> combine
makeContractedForm: TripleForm SelectAtom -> ContractedForm SelectAtom
makeContractedForm triples = triples
                            |> List.map (\(s, p, o) -> (s, (p, o)))
                            |> List.sortBy (\(x, y) -> x.value)
                            |> groupWhile (\a b -> (Tuple.first a).value == (Tuple.first b).value)
                            |> separateIntoSubject_PredicateObjects
                            |> List.map (\(x, y) -> (x, List.sortBy (\(a, b) -> a.value) y 
                                                        |> groupWhile (\a b -> (Tuple.first a).value == (Tuple.first b).value)
                                                        |> separateIntoPredicateLists
                                ))

-- List.indexedMap Tuple.pair a |>                            -- Dict.fromList  will make an Dict
-- List.map (\x -> (Tuple.second x, Tuple.first x)) c

contractResult: ServerVars -> ServerForm SelectAtom -> Maybe (ContractedForm SelectAtom)
contractResult vars res =
-- ContractedForm is only valid when ServerVars in the shape of spo
    case vars of
        ["s", "p", "o"] -> 
            let
                maybeTriples = makeTripleForm res
            in
                case maybeTriples of
                   Just a -> makeContractedForm a |> Just
                   _ -> Nothing
        _ -> Nothing

selectAtom2RdfNode: SelectAtom -> RdfNode
selectAtom2RdfNode atom =
    case atom.aType of
        "uri" -> Uri {value=atom.value}
        "bnode" -> BlankNode {value=atom.value}
        "literal" -> 
            if atom.language /= ""
            then
                LiteralValueAndLanguageString {value=atom.value, language=atom.language}
            else if atom.datatype /= ""
                then LiteralValueAndDataType {value=atom.value, dataType=atom.datatype}
                else LiteralOnlyValue {value=atom.value}
        _ -> Unknown


makeRdfKey: RdfNode -> Maybe RdfKey
makeRdfKey n =
    case n of
        Uri a -> 
            Just a.value
        BlankNode a ->
            Just a.value
        _ -> Nothing       


subjectMoleculeMap: (SelectAtom -> RdfNode) -> SubjectMolecule SelectAtom -> SubjectMolecule RdfNode
subjectMoleculeMap fn (subj, po) =
    (fn subj, (List.map (\(p, lo) ->
                            (fn p, List.map(\o -> fn o) lo)
                        ) po)
    )

makeRdfDict: ContractedForm SelectAtom -> RdfDict
makeRdfDict cf = List.map (\subjM ->
                                let
                                    key = subjM |> Tuple.first |> selectAtom2RdfNode |> makeRdfKey |> Maybe.withDefault "unidentifiable"
                                in
                                    (key, subjectMoleculeMap selectAtom2RdfNode subjM)
                            ) cf
                |> Dict.fromList
rdfNodeToMaybeString: RdfNode -> Maybe String
rdfNodeToMaybeString node =
    case node of
        Uri a -> Just a.value
        BlankNode a -> Just a.value
        LiteralOnlyValue a -> Just a.value
        LiteralValueAndDataType a -> Just a.value
        LiteralValueAndLanguageString a -> Just a.value
        Unknown -> Nothing

extractValues: List SelectAtom -> List String
extractValues rows =
    List.map (\r -> r.value) rows


makeTriple: List a -> Maybe (a, a, a)
makeTriple spo =
    case (List.length spo) of
       3 -> Maybe.map3 (\a b c -> (a, b, c)) (List.head spo) (List.head (List.drop 1 spo)) (List.head (List.drop 2 spo))
       _ -> Nothing

separateIntoSubject_PredicateObjects: List (( a, (a, a) )
                                            , List ( a, (a, a) )) 
                                            -> List (a, List(a, a))
separateIntoSubject_PredicateObjects l = 
    l |> List.map (\subject ->
                        let
                            subj = Tuple.first (Tuple.first subject)
                            predobj = Tuple.second (Tuple.first subject)
                            predicateObjects = predobj :: Tuple.second (List.unzip (Tuple.second subject))

                        in
                            (subj, predicateObjects)
                    )

separateIntoPredicateLists: List ( ( a, a ), List ( a, a ) ) -> List (a, List(a))
separateIntoPredicateLists preds = 
                                preds |> List.map(\p ->
                                    let
                                        item = Tuple.first (Tuple.first p)
                                        head = Tuple.second (Tuple.first p)
                                        target = head :: Tuple.second (List.unzip (Tuple.second p))
                                    in
                                        (item, target)
                                ) 
