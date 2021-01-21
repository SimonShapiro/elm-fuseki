module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, h2, h4, span, ul, li, b, p, hr, br, table, tr, th, td, small)
import Html exposing (textarea, a, img)
import Html.Attributes exposing (href, placeholder, value, class, rows, cols, wrap, style, type_, name, checked, src, target)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (pushUrl, replaceUrl, load, Key)
import Http
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, Error, errorToString, field, string, map, map2, map3, map4, map5, map6, map7, list, int, decodeString, at, andThen)

import File exposing (File)
import File.Select as Select
import File.Download as Download
import Task
import List.Extra exposing (uncons, groupWhile)
import Maybe.Extra exposing (combine, join)
import List
import Dict exposing(Dict)
import Url exposing (..)
import Url.Builder exposing (relative)
import Url.Parser.Query as Query
import Url.Parser exposing (Parser, (<?>), s)
import Url.Parser exposing (query)
import Task exposing (succeed)
import Regex exposing (Regex)
import Html exposing (select)

import Graph
import Json.Decode exposing (index)
import String

graph1 = Graph.fromNodesAndEdges[Graph.Node 1 (Select "")][Graph.Edge 1 1 ""]

type SparqlQuery 
    = Select Sparql
    | Ask Sparql
    | Construct Sparql
    | Describe Sparql
    | Unrecognised

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }
type Cardinality 
    = OneToOne 
    | ZeroOrOne 
    | ZeroToMany 
    | OneToMany 

type alias ServerForm a = (List (List a))

type alias ServerVars = (List String)

type alias TripleForm a = (List (a, a, a))

type alias ContractedForm a = (List (SubjectMolecule a)) 

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

type UIState 
    = Initialising 
    | Pinging 
    | Querying 
    | DisplayingSelectResult  ServerVars (ServerForm SelectAtom)
    | DisplayingSelectError String
    | ApiError Http.Error
    | Waiting

type alias Model = 
    { state: UIState
    , server: Server
    , urlQuery: Maybe Sparql
    , query: Sparql 
    , keyboard: KeyboardMode
    , resultsDisplay: ResultsDisplay
    , predicateStyle: PredicateStyle
    , openPredicatesInSubject: OpenPredicatesInSubject
    , key: Key
    }

type alias OpenPredicatesInSubject = List (RdfNode, RdfNode)
type alias SubjectMolecule a = (a, (List (a, List a)))  -- s, List (pred, obj)

type KeyboardMode
    = Normal
    | Ctrl
    | ReadyToAcceptControl

type ResultsDisplay
    = Table
    | SubjectOrientation

type PredicateStyle
    = Terse
    | Verbose

type Msg 
    = NoOp
    | PressedKey String
    | ChangeServer Server
    | PingServer 
    | Pinged (Result Http.Error ())
    | ChangeQuery Sparql
    | SubmitQuery Sparql
    | SubmitQueryWhileNavigating Sparql
    | GotSparqlResponse (Result Http.Error KGResponse)
    | FileRequested 
    | FileSelected File
    | FileLoaded Sparql
    | DownloadFile
    | DownloadResultsAsCSV ServerVars (ServerForm SelectAtom)
    | ChangeOutputFormat String
    | ChangePredicateStyle String
    | BackToQuery
    | RegisterSubjectPredicateOpen (RdfNode, RdfNode)
    | DeregisterSubjectPredicateOpen (RdfNode, RdfNode)
    | ClickedLink UrlRequest

type alias Server = String

type alias Sparql = String

type alias KGResponse =  -- a copy of the query is available in the api
    { server: Server
    , status: Int
    , message: String
    , queryType: String
    , query: Sparql
    , vars: ServerVars
    , result: ServerForm SelectAtom
    }

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
establishQueryType: Sparql -> SparqlQuery
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

type alias RdfKey = String

makeRdfKey: RdfNode -> Maybe RdfKey
makeRdfKey n =
    case n of
        Uri a -> 
            Just a.value
        BlankNode a ->
            Just a.value
        _ -> Nothing       

type alias RdfDict = Dict RdfKey (SubjectMolecule RdfNode)  -- the main form of the graph -- consider replacing with elm-community/graph

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

convertRdfDict2CommunityGraph: RdfDict -> Graph.Graph (SubjectMolecule RdfNode) String 
convertRdfDict2CommunityGraph d = 
--(List (Graph.Node (SubjectMolecule RdfNode)), Dict String Int) 
    let
        (index, values) = Dict.toList d
                            |> List.unzip
        reverseDict = List.indexedMap Tuple.pair index
                        |> List.map(\(x, y) -> (y, x))
                        |> Dict.fromList
        nodes = List.indexedMap Tuple.pair values
                |> List.map (\(x, y) -> Graph.Node x y)

        edges = List.map (\(k, node) ->
                            let
                                id = Maybe.withDefault -1 <| Dict.get k reverseDict
                                spo = Tuple.second node
                            in
                            List.map (\(p, po) ->
                                List.map (\o ->
                                    Dict.get (Maybe.withDefault "???" <| rdfNodeToMaybeString o) reverseDict
                                ) po
                                |> Maybe.Extra.values
                                |> List.map (\e -> Graph.Edge id e (Maybe.withDefault "???" <| rdfNodeToMaybeString p)) 
                            ) spo
                            |> List.concat
                        ) (Dict.toList d)
                        |> List.concat
                
    in
        Debug.log ("Inside conversion"++(List.length values |> String.fromInt))
        Debug.log ("Building graph of "++(List.length nodes |> String.fromInt)++":"++(List.length edges |> String.fromInt))
        Graph.fromNodesAndEdges nodes edges

extractValues: List SelectAtom -> List String
extractValues rows =
    List.map (\r -> r.value) rows

selectAtomDecoder: Decoder SelectAtom
selectAtomDecoder = 
    map5 SelectAtom
        (field "key" string)
        (field "value" string )
        (field "aType" string )
        (field "language" string )
        (field "datatype" string )

server: Server
server = "http://localhost:port"

-- startWith: Model
-- startWith = Model Initialising server "" Normal Table Terse [] 

initialFn: flags -> Url -> Key -> (Model, (Cmd Msg))
initialFn _ url key =
    let
        initialQuery = parseUrlForIndexQuery url
    in
        (Model Initialising server initialQuery "" Normal Table Terse [] key, Cmd.none)

parseUrlForDetailsSubject : Url -> Maybe String
parseUrlForDetailsSubject url =
    let
        subject: Query.Parser (Maybe String)
        subject = 
            Query.string "subject"
        parseQuery =
            (s "details" <?> subject)
    in
        Url.Parser.parse parseQuery url |> join

parseUrlForIndexQuery : Url -> Maybe String
parseUrlForIndexQuery url =
    let
        subject: Query.Parser (Maybe String)
        subject = 
            Query.string "query"
        parseQuery =
            (s "index.html" <?> subject)
    in
        Url.Parser.parse parseQuery url |> join

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    case parseUrlForIndexQuery url of
                        Nothing ->
                            Debug.log ("Internal update running on NOTHING")    --(Url.toString url)) 
                            ( model
                            , replaceUrl model.key (Url.toString url) 
                            )
                        Just a -> 
                            Debug.log ("Internal update running "++ a)    --(Url.toString url)) 
                            ( {model | query = a}
                            , pushUrl model.key (relative [][Url.Builder.string "query" a]) 
                            )
                External url ->
                    ( model
                    , load url
                    )
        BackToQuery -> ({model | state = Querying}, Cmd.none)
        PressedKey s -> 
            case model.keyboard of
                ReadyToAcceptControl -> 
--                    Debug.log ("Key = "++s)
--                    (model, Cmd.none)
                    case s of
                        "Control" ->
                            Debug.log "Entering Ctrl mode" 
                            ({model | keyboard = Ctrl}, Cmd.none)
                        "{" -> 
                            Debug.log "Opening brace" 
                            ({model | query = model.query++"}"}, Cmd.none)
                        _ -> (model, Cmd.none)
                Ctrl ->
                    case s of 
                        "Shift" -> 
                            Debug.log ("In Ctrl+F2 mode "++s)
                            ({model | keyboard = ReadyToAcceptControl, state = Waiting}
                            , submitParametrisedQuery model.server 
                               """select distinct ?domain ?predicate {
                                    ?s ?predicate ?o.
                                    ?s a ?domain .
                                    } order by ?domain ?predicate
                                """ (Http.expectJson GotSparqlResponse mainDecoder))
                        _ ->
                            Debug.log ("Leaving Ctrl mode "++s)
                            ({model | keyboard = ReadyToAcceptControl}, Cmd.none)
                Normal -> 
                    ({model | keyboard = Normal}, Cmd.none)
--            ({model | query = model.query++s}, Cmd.none )
        ChangeServer newServer -> ({model | server = newServer}, Cmd.none)
        PingServer -> ({model | state = Pinging}, pingServer model.server)
        Pinged result ->
            case result of
                Ok _ -> 
                    case model.urlQuery of
                        Nothing ->
                            Debug.log "Pinged OK - no initial query"
                            ({model | state = Querying, keyboard = ReadyToAcceptControl}, Cmd.none)
                        Just query -> 
                            Debug.log ("Pinged OK with"++query)
                            (   { model | state = Querying 
                                , keyboard = ReadyToAcceptControl
                                , query = query
                                }
                            , submitQuery model.server (establishQueryType query))
                Err e -> 
                    Debug.log "Pinged ERROR"
                    ({model | state = Initialising}, Cmd.none)
        ChangeQuery newQuery -> 
            case model.keyboard of
                ReadyToAcceptControl ->
                    Debug.log ("Query "++newQuery)
                    ({model | query = newQuery, state = Querying}, Cmd.none)
                _ ->
                    (model, Cmd.none) 
        SubmitQuery query -> 
            case establishQueryType model.query of
               Unrecognised -> ({model | state = ApiError (Http.BadBody ("I don't recognise this query type "++model.query))}, Cmd.none)
               _ ->
                    Debug.log ("Submitting Query "++model.query)
                    ({model | state = Waiting}, pushUrl model.key (relative [][Url.Builder.string "query" query])) -- submitQuery model.server query)
        SubmitQueryWhileNavigating query ->
            let
                newModel = {model | query = query, state = Waiting}
            in
                Debug.log newModel.query
                (newModel, submitQuery newModel.server (establishQueryType newModel.query))
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    case okData.status of
                        200 ->
                            -- could transform to graph here
                            ({model | state = DisplayingSelectResult okData.vars okData.result}, Cmd.none)
                        _ ->
                            ({model | state = DisplayingSelectError okData.message}, Cmd.none)
                Err e -> 
                    Debug.log "Response ERROR"
                    ({model | state = ApiError e}, Cmd.none)
        FileRequested  ->
                ( model
                , Select.file ["text"] FileSelected
                )  --          (Querying newServer "", Cmd.none)
        FileSelected file ->        
                ( model
                , Task.perform FileLoaded (File.toString file)
                )
        FileLoaded content ->
            ({model | query = content}, Cmd.none)
        DownloadFile -> 
            (model, downloadFile "query.txt" model.query)
        DownloadResultsAsCSV vars results -> 
            let
                headedResults = (String.join "|" vars) :: List.map (\r -> extractValues r
                                                        |> String.join "|"
                                                 ) results
            in
                (model, downloadFile "result.csv" <| String.join "\n" headedResults)
        ChangeOutputFormat outputFormat ->
            case outputFormat of
                "table" -> ({model | resultsDisplay = Table}, Cmd.none)
                "subject" -> ({model | resultsDisplay = SubjectOrientation}, Cmd.none)
                _ -> ({model | resultsDisplay = Table}, Cmd.none)
        ChangePredicateStyle predicateStyle ->
            case predicateStyle of
               "verbose" -> ({model | predicateStyle = Verbose}, Cmd.none)
               "terse" -> ({model | predicateStyle = Terse}, Cmd.none)
               _ -> ({model | predicateStyle = Verbose}, Cmd.none)
        RegisterSubjectPredicateOpen selected -> 
            ({model | openPredicatesInSubject = selected::model.openPredicatesInSubject}, Cmd.none)
        DeregisterSubjectPredicateOpen selected -> 
            ({model | openPredicatesInSubject = List.Extra.remove selected model.openPredicatesInSubject}, Cmd.none)

handleUrlRequest: UrlRequest -> Msg
handleUrlRequest req = 
    case req of
        Internal url ->
            Debug.log ("Handling internal request to "++(Url.toString url))
            ClickedLink req
        External url ->
            Debug.log ("Handling external request to "++url)
            ClickedLink req


handleUrlChange: Url -> Msg
handleUrlChange url = 
    case (parseUrlForIndexQuery url) of -- have to reparse url
        Nothing ->
            Debug.log ("fwd/bck update running on NOTHING")    --(Url.toString url)) 
            NoOp
        Just a -> 
            Debug.log ("fwd/bck update running "++ a)    --(Url.toString url)) 
            SubmitQueryWhileNavigating  a

msgDecoder : Decoder Msg
msgDecoder =
    field "key" string
        |> map PressedKey

subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown msgDecoder

pingServer : Server -> (Cmd Msg)
pingServer newServer = 
  --  let
        Debug.log ("Pinging"++newServer)

  --  in
        Http.request
                    { method = "HEAD"
                    , headers = []
                    , url = newServer++"/hello"
                    , body = Http.stringBody "text" ""
                    , expect = Http.expectWhatever Pinged
                    , timeout = Nothing
                    , tracker = Nothing
                    }

prepareHttpRequest: Server -> String -> String -> String -> (Cmd Msg)
prepareHttpRequest newServer header qtype query =
    Http.request
                { method = "POST"
                , headers = [ Http.header "Content-Type" "application/sparql-request"
                            , Http.header "Accept" header
                            , Http.header "x-Qtype" qtype
                            ]
                , url = newServer++"/sparql"
                , body = Http.stringBody "text" query
                , expect = Http.expectJson GotSparqlResponse mainDecoder -- this could be parameterised
                , timeout = Nothing
                , tracker = Nothing
                }


submitQuery: Server -> SparqlQuery -> (Cmd  Msg)
submitQuery newServer sparql = 
    case sparql of
        Select query -> prepareHttpRequest newServer "application/json" "select" query
        Ask query -> prepareHttpRequest newServer "application/json" "ask" query
        Construct query -> prepareHttpRequest newServer "application/ld+json" "construct" query
        Describe query -> prepareHttpRequest newServer "application/ld+json" "describe" query
        Unrecognised -> Task.perform (always NoOp) (Task.succeed ()) 

submitParametrisedQuery: Server -> Sparql -> (Http.Expect msg) -> (Cmd Msg)
submitParametrisedQuery newServer query returnTo = 
        Debug.log ("using "++newServer)
        prepareHttpRequest newServer "application/json" "select" query

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

aka: PredicateStyle -> String -> String
aka predicateStyle pred = 
    case predicateStyle of
       Verbose -> pred
       Terse -> String.split "/" pred
                |> List.reverse
                |> List.head
                |> Maybe.withDefault pred

-- pivotToSubject: ServerForm -> List (List (SelectAtom, (List SelectAtom)))   --   (subject, [pred, obj])
-- pivotToSubject results =
--     let
--         subjects = List.filterMap (\triple -> uncons triple) results -- List (SelectAtom, List SelectAtom) ie ?s of [?s ?p ?o])
--             |> groupWhile (\a b -> (Tuple.first a).value == (Tuple.first b).value)
-- --            |> List.map (\group -> (Tuple.first group)::(Tuple.second group))
-- --            |> List.map (\subject -> List.map (\group -> makeTriple group) subject) 
--     in
--         -- Debug.log (String.join ";" (List.map (\s -> s.value) subjects))
--         subjects
    
viewSubjects: OpenPredicatesInSubject -> PredicateStyle -> RdfDict -> Html Msg 
viewSubjects openPredicates predicateStyle subjs =
        div []
        (List.map (\spoKey ->
            case (Dict.get spoKey subjs) of
                Nothing ->            div [ class "container"]
                                [ text "Lost subject molecule"]
                Just a ->                div [ class "container"]
                            [ viewSubjectMolecule openPredicates predicateStyle a]
        ) (Dict.keys subjs))


viewSubjectMolecule: OpenPredicatesInSubject -> PredicateStyle -> (SubjectMolecule RdfNode) -> Html Msg
viewSubjectMolecule openPredicates predicateStyle mole =
    let
        subj = Tuple.first mole
    in
              div [class "card"]
                                [ h2 [] [ Html.a [href ("/index.html?query=describe <"
                                    ++ (makeRdfKey subj |> Maybe.withDefault "unknown") 
                                    ++">")][(viewRdfNode subj)]] -- make case here to clean up the view function below
                                , viewPredicates openPredicates predicateStyle mole
                                ]

viewPredicates: OpenPredicatesInSubject -> PredicateStyle -> SubjectMolecule RdfNode -> Html Msg
viewPredicates openPredicates predicateStyle mole =
    let
        subj = Tuple.first mole
        preds = Tuple.second mole
    in
        div []
            (List.map(\po -> 
                div []
                    [ viewRdfNodeAsPredicate predicateStyle (Tuple.first po)
                    , viewObjects openPredicates subj po
                    ]
            )
            preds)

viewRdfNodeAsPredicate: PredicateStyle -> RdfNode -> Html Msg
viewRdfNodeAsPredicate predicateStyle node = 
    case node of
        Uri a ->
            b []    [ text (aka predicateStyle a.value)
                    , text ": "
                    ]
        _ ->
            b []    [ text "All predicates should be Uri"]

removeUrlFragment: String -> String
removeUrlFragment urlString =
    urlString 
    |> String.split "#"
    |> List.head
    |> Maybe.withDefault urlString 

encodeUrlFragmentMarker: String -> String
encodeUrlFragmentMarker urlString =
    String.replace "#" "%23" urlString

viewRdfNode: RdfNode -> Html Msg
viewRdfNode node = 
    case node of
        Uri a ->
            span [] [ Html.a [href ("/index.html?query=describe <"++(encodeUrlFragmentMarker a.value)++">")][text a.value]  -- remove fragment on anchor
                    , Html.a [href a.value, target "_blank"][img [src "www-12px.svg"][]]
                    ]
        BlankNode a ->
            text a.value
        LiteralOnlyValue a ->
            text a.value
        LiteralValueAndDataType a ->
            span [] [ text a.value
                    , small [] [text "  (", text a.dataType, text ")"]]
        LiteralValueAndLanguageString a ->
            span [] [ text a.value
                    , small [] [text "  (", text a.language, text ")"]]
        Unknown ->
            b []    [ text "Unrecognised Atom"]

viewObjects: OpenPredicatesInSubject -> RdfNode -> (RdfNode, List RdfNode) -> Html Msg
viewObjects open subj po =
    let
        (pred, objs) = po
        head = List.head objs
        rest = List.tail objs
                |> Maybe.withDefault []
        restCount = List.length rest
    in
        case head of
           Just obj -> case restCount of
                                0 -> div [] [viewRdfNode obj]
                                _ -> div [] (viewRestOfObjectList open (subj, pred) obj rest)

           Nothing -> text ""

viewRestOfObjectList: OpenPredicatesInSubject -> (RdfNode, RdfNode) -> RdfNode -> List RdfNode -> List (Html Msg)
viewRestOfObjectList open selected obj rest =
    case (List.Extra.find (\o -> o == selected) open) of
        Just a -> viewRdfNode obj
                  :: button [onClick (DeregisterSubjectPredicateOpen selected)] [text " less"]
                  :: (List.map (
                        \r -> div [] [viewRdfNode r]
                    ) rest)
        Nothing -> [ viewRdfNode obj
                    , button [onClick (RegisterSubjectPredicateOpen selected)] [text ((String.fromInt <| List.length rest)++" more")]
                    ]
-- View


-- could refactor below into two steps - shape and show
tableView: ServerVars -> ServerForm SelectAtom -> Html Msg
tableView vars result =
        div [] 
            [ table []
                ((tr [] (List.map (\v -> th[][text v]) vars))
                ::(List.map (
                        \row ->
                        tr []
                            (List.map (
                                \var -> 
                                    td [][ text var]
                            ) (extractValues row))
                    ) result ))
            ]

queryInput: Server -> Sparql -> Html Msg
queryInput newServer query =
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
resultFormatToggle: ResultsDisplay -> Html Msg
resultFormatToggle selected = 
    div []
        [ text "Output format"
        , input [ type_ "radio"
                , name "resultFormat"
                , value "table"
                , checked (selected == Table)
                , onInput ChangeOutputFormat
                ][]
        , text "Table"
        , input [ type_ "radio"
                , name "resultFormat"
                , value "subject"
                , checked (selected == SubjectOrientation)
                , onInput ChangeOutputFormat
                ][]
        , text "Subjects"
        ]

predicateStyleToggle: PredicateStyle -> Html Msg
predicateStyleToggle selected =
    div []
        [ text "Output format"
        , input [ type_ "radio"
                , name "predicateStyle"
                , value "verbose"
                , checked (selected == Verbose)
                , onInput ChangePredicateStyle
                ][]
        , text "Verbose"
        , input [ type_ "radio"
                , name "predicateStyle"
                , value "terse"
                , checked (selected == Terse)
                , onInput ChangePredicateStyle
                ][]
        , text "Terse"
        ]


uploadQueryFromFile:  Html Msg
uploadQueryFromFile = 
    div []
    [ button [onClick FileRequested][text "Load query"]
    , button [onClick DownloadFile][text "Download query"]]

downloadFile: String -> String -> Cmd msg
downloadFile fileName query =
  Download.string fileName "text" query

view: Model -> Document Msg
view model = { title = "Sparql Query Playground"
            , body = (List.singleton <|
                    case model.state of
                        Initialising ->
                            div [] 
                                [ h1 [][a [href "http://www.cnn.com"][text "hello world"]]
                                , div [] 
                                    [ input [placeholder "Server", onInput ChangeServer, value model.server][]
                                    , button [onClick PingServer][text "Connect"]
                                    ]
                                ]
                        Pinging -> 
                            div [][text <| "Pinging"++model.server]
                        Querying -> div []
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , resultFormatToggle model.resultsDisplay
                                ]
                        Waiting -> div [style "cursor" "progress"]
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , b [] [text "Wating for server response..."]
                                ]
                        ApiError error -> 
                            case error of
                                Http.BadBody err ->
                                    div []
                                        [ h1 [][text "ApiError: I can't interpret the response"]
                                        , div [][text err]
                                        , button [onClick BackToQuery][text "Back"]
                                        ]
                                _ ->
                                    div [][
                                        h1 [][text "ApiError: Oops - something went wrong! :-("]
                                        , button [onClick BackToQuery][text "Back"]
                                    ]
                        DisplayingSelectError message ->
                            div []                
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , div [][text message]
                                ]
                        DisplayingSelectResult vars result ->
                            case model.resultsDisplay of
                                Table ->
                                    div [class "main"]                
                                        [ uploadQueryFromFile
                                        , queryInput model.server model.query
                                        , resultFormatToggle model.resultsDisplay
                                        , button [onClick <| DownloadResultsAsCSV vars result][text "Download csv"]
                                        , tableView vars result
                                        ]
                                SubjectOrientation ->
                                    let
                                        contracted = contractResult vars result  -- Maybe (ContractedForm SelectAtom)
                                                    |> Maybe.map makeRdfDict     -- Maybe RdfDict
                                    in
                                    case contracted of
                                        Just a ->
                                            div [class "main"]                
                                                [ uploadQueryFromFile
                                                , queryInput model.server model.query
                                                , resultFormatToggle model.resultsDisplay
                                                , h2 [][text "Subject orientation"]
                                                , predicateStyleToggle model.predicateStyle
                                                , br [] []
                                                , viewSubjects model.openPredicatesInSubject model.predicateStyle a
                                                , hr [] []
                                                , div []
                                                    (List.map (\n -> viewSubjectMolecule model.openPredicatesInSubject model.predicateStyle n.label) 
                                                                            (Graph.nodes (convertRdfDict2CommunityGraph a)))
                                                ]
                                        Nothing ->                             
                                            div []                
                                                [ uploadQueryFromFile
                                                , queryInput model.server model.query
                                                , resultFormatToggle model.resultsDisplay
                                                , h4 [][text "Subject orientation only where results are in the shape of ?s ?p ?o"]
                                                ]
            )}

-- viewGraphNode: Graph.Node (SubjectMolecule RdfNode) -> Html Msg
-- viewGraphNode n =
--      viewSubjectMolecule n

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
  Browser.application
    { init = initialFn
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = handleUrlChange
    , onUrlRequest = handleUrlRequest
    }