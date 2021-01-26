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
--import Task
import List
import Dict exposing(Dict)
import Maybe.Extra exposing (combine, join)
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

import Sparql exposing (..)
import RdfDict exposing (..)
import List.Extra exposing (uncons, groupWhile)

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }
type Cardinality 
    = OneToOne 
    | ZeroOrOne 
    | ZeroToMany 
    | OneToMany 


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
    , urlQuery: Maybe SparqlQuery
    , query: SparqlQuery
    , currentRdfDict: Maybe RdfDict
    , keyboard: KeyboardMode
    , resultsDisplay: ResultsDisplay
    , predicateStyle: PredicateStyle
    , openPredicatesInSubject: OpenPredicatesInSubject
    , key: Key
    }

type alias OpenPredicatesInSubject = List (RdfNode, RdfNode)

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

type ViewRdfNodeAs
    = Subject
    | Predicate
    | Object

type Msg 
    = NoOp
    | PressedKey String
    | ChangeServer Server
    | PingServer 
    | Pinged (Result Http.Error ())
    | ChangeQuery String
    | SubmitQuery SparqlQuery
    | SubmitQueryWhileNavigating SparqlQuery
    | GotSparqlResponse (Result Http.Error KGResponse)
    | FileRequested 
    | FileSelected File
    | FileLoaded String
    | DownloadFile
    | DownloadResultsAsCSV ServerVars (ServerForm SelectAtom)
    | ChangeOutputFormat String
    | ChangePredicateStyle String
    | BackToQuery
    | RegisterSubjectPredicateOpen (RdfNode, RdfNode)
    | DeregisterSubjectPredicateOpen (RdfNode, RdfNode)
    | ClickedLink UrlRequest

type alias KGResponse =  -- a copy of the query is available in the api
    { server: Server
    , status: Int
    , message: String
    , queryType: String
    , query: String
    , vars: ServerVars
    , result: ServerForm SelectAtom
    }
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

server: Server
server = "http://localhost:port"

-- startWith: Model
-- startWith = Model Initialising server "" Normal Table Terse [] 

initialFn: flags -> Url -> Key -> (Model, (Cmd Msg))
initialFn _ url key =
    let
        initialQuery = parseUrlForIndexQuery url
    in
        (Model Initialising server initialQuery (Ask "ask {?s ?p ?o}") Nothing Normal Table Terse [] key, Cmd.none)

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

parseUrlForIndexQuery : Url -> Maybe SparqlQuery
parseUrlForIndexQuery url =
    let
        subject: Query.Parser (Maybe String)
        subject = 
            Query.string "query"
        parseQuery =
            (s "index.html" <?> subject)
    in
        Url.Parser.parse parseQuery url |> join |> Maybe.map establishQueryType

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
                            let
                                _ = Debug.log "Internal update running " a    --(Url.toString url)) 
                            in
                                ( {model | query = a}
                                , pushUrl model.key (relative [][Url.Builder.string "query" (Sparql.toString a)]) 
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
                        _ -> (model, Cmd.none)
                Ctrl ->
                    case s of 
                        "Shift" -> 
                            let
                                queryString = """select distinct ?domain ?predicate {
                                    ?s ?predicate ?o.
                                    ?s a ?domain .
                                    } order by ?domain ?predicate
                                        """
                                command = submitQuery model.server (Sparql.Select queryString) (Http.expectJson GotSparqlResponse mainDecoder)
                                _ = Debug.log ("In Ctrl+F2 mode "++s)
                            in
                                ({model | keyboard = ReadyToAcceptControl, state = Waiting}, command)
--                            , 
--                            )
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
                            let
                                command = submitQuery model.server query (Http.expectJson GotSparqlResponse mainDecoder)
                                _ = Debug.log "Pinged OK with" query

                            in
                                ({ model | state = Querying, keyboard = ReadyToAcceptControl, query = query}, Cmd.none)
                Err e -> 
                    Debug.log "Pinged ERROR"
                    ({model | state = Initialising}, Cmd.none)
        ChangeQuery newQuery -> 
            case model.keyboard of
                ReadyToAcceptControl ->
                    Debug.log ("Query "++newQuery)
                    ({model | query = (establishQueryType newQuery), state = Querying}, Cmd.none)
                _ ->
                    (model, Cmd.none) 
        SubmitQuery query -> 
            case model.query of
               Unrecognised newQuery -> ({model | state = ApiError (Http.BadBody ("I don't recognise this query type "++(Sparql.toString model.query)))}, Cmd.none)
               _ ->
                --    Debug.log ("Submitting Query " model.query
                    ({model | state = Waiting}, pushUrl model.key (relative [][Url.Builder.string "query" (Sparql.toString query)])) -- submitQuery model.server query)
        SubmitQueryWhileNavigating query ->
            let
                newModel = {model | query = query, state = Waiting}
                _ = Debug.log "query=" newModel.query
            in
                (newModel, submitQuery newModel.server newModel.query (Http.expectJson GotSparqlResponse mainDecoder))
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    case okData.status of
                        200 ->
                            -- could transform to graph here
                            ({ model | state = DisplayingSelectResult okData.vars okData.result
                             , currentRdfDict = contractResult okData.vars okData.result  -- Maybe (ContractedForm SelectAtom)
                                                |> Maybe.map makeRdfDict 
                             }, Cmd.none)
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
            ({model | query = establishQueryType content}, Cmd.none)
        DownloadFile -> 
            (model, downloadFile "query.txt" (Sparql.toString model.query))
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
            let
                _ = Debug.log "fwd/bck update running " a    --(Url.toString url)) 
            in
                SubmitQueryWhileNavigating  a

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

aka: PredicateStyle -> String -> String
aka predicateStyle pred = 
    case predicateStyle of
       Verbose -> pred
       Terse -> String.split "/" pred
                |> List.reverse
                |> List.head
                |> Maybe.withDefault pred

viewSubjects: Model -> Html Msg 
viewSubjects model =
    case model.currentRdfDict of
       Nothing -> div [][text "rdfdict erro"]
       Just a ->
            div []
            (List.map (\spo ->
                    div [ class "container"]
                                [ viewSubjectMolecule model spo]
            ) (Dict.values a))

makeSubjectMoleculeCard: Model -> (SubjectMolecule RdfNode) -> Html Msg
makeSubjectMoleculeCard model mole =    
    let
        subj = Tuple.first mole
    in
              div [class "card"]
                                [ h2 [] [ Html.a [href ("/index.html?query=describe <"
                                    ++ (makeRdfKey subj |> Maybe.withDefault "unknown") 
                                    ++">")][(viewRdfNode model Subject subj)]] -- make case here to clean up the view function below
                                , viewPredicates model mole
                                ]
 
viewSubjectMolecule: Model -> (SubjectMolecule RdfNode) -> Html Msg
viewSubjectMolecule model mole =
    let
        subj = Tuple.first mole
    in
        case subj of
           BlankNode a -> span [] [] -- "blank node detected"
           _ -> makeSubjectMoleculeCard model mole

expandObjectInPlace: Model -> RdfKey -> Html Msg
expandObjectInPlace k dict =
    span [] []

viewPredicates: Model -> SubjectMolecule RdfNode -> Html Msg
viewPredicates model mole =
    let
        subj = Tuple.first mole
        preds = Tuple.second mole
    in
        div []
            (List.map(\po -> 
                div []
                    [ viewRdfNode model Predicate (Tuple.first po)
                    , viewObjects model subj po
                    ]
            )
            preds)

viewRdfNodeAsPredicate: Model -> RdfNode -> Html Msg
viewRdfNodeAsPredicate model node = 
    case node of
        Uri a ->
            b []    [ text (aka model.predicateStyle a.value)
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

viewRdfNode: Model -> ViewRdfNodeAs -> RdfNode -> Html Msg
viewRdfNode model nodeType node = 
    case node of
        Uri a ->
            case nodeType of
                Object ->
                    span [] [ Html.a [href ("/index.html?query=describe <"++(encodeUrlFragmentMarker a.value)++">")][text a.value]  -- remove fragment on anchor
                            , Html.a [href a.value, target "_blank"][img [src "www-12px.svg"][]]
                            ]
                Subject -> span [][text a.value]
                Predicate -> 
                    viewRdfNodeAsPredicate model node --text a.value -- makeSubjectMoleculeCard model node --text a.value  -- a is now an RdfKey and can be expanded via Model
        BlankNode a ->
            case model.currentRdfDict of
               Nothing -> text a.value
               Just dict ->
                    let
                        related = Dict.get a.value dict
                    in
                     case related of
                        Nothing -> span [][text a.value]
                        Just subjectMole -> 
--                            Debug.log ("Going after "++a.value)
                            case nodeType of
                                Object ->
                                    makeSubjectMoleculeCard model subjectMole --text a.value  -- a is now an RdfKey and can be expanded via Model
                                Subject -> span [][text a.value]
                                Predicate -> viewRdfNode model Predicate node
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

viewObjects:  Model -> RdfNode -> (RdfNode, List RdfNode) -> Html Msg
viewObjects model subj po =
    let
        (pred, objs) = po
        head = List.head objs
        rest = List.tail objs
                |> Maybe.withDefault []
        restCount = List.length rest
    in
        case head of
           Just obj -> case restCount of
                                0 -> div [] [viewRdfNode model Object obj]
                                _ -> div [] (viewRestOfObjectList model (subj, pred) obj rest)

           Nothing -> text ""

viewRestOfObjectList: Model -> (RdfNode, RdfNode) -> RdfNode -> List RdfNode -> List (Html Msg)
viewRestOfObjectList model selected obj rest =
    case (List.Extra.find (\o -> o == selected) model.openPredicatesInSubject) of
        Just a -> viewRdfNode model Object obj
                  :: button [onClick (DeregisterSubjectPredicateOpen selected)] [text " less"]
                  :: (List.map (
                        \r -> div [] [viewRdfNode model Object r]
                    ) rest)
        Nothing -> [ viewRdfNode model Object obj
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

queryInput: Server -> SparqlQuery -> Html Msg
queryInput newServer query =
            div [] 
                [ textarea 
                    [ cols 120
                    , rows 15
                    , wrap "soft"
                    , placeholder "Sparql Query"
                    , onInput ChangeQuery
                    , value (Sparql.toString query)
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
                                                -- ??? replace below by passing the whole model in
                                                , viewSubjects model
                                                , hr [] []
                                                , div [][text "Graph nav (off)"]
--                                                , div []
 --                                                   (List.map (\n -> viewSubjectMolecule model n.label) 
  --                                                                          (Graph.nodes (convertRdfDict2CommunityGraph a)))
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

msgDecoder : Decoder Msg
msgDecoder =
    field "key" string
        |> map PressedKey

selectAtomDecoder: Decoder SelectAtom
selectAtomDecoder = 
    map5 SelectAtom
        (field "key" string)
        (field "value" string )
        (field "aType" string )
        (field "language" string )
        (field "datatype" string )

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
