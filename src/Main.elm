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

import Element exposing (..)
import Element.Border exposing (..)
import Element.Background exposing (..)
import Element.Input exposing (..)
import Element.Font exposing (..)
import Dict
import List.Extra
import Element
import Element
import Element
import Element
import Element
import Element

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
    , queryHistory: List SparqlQuery
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
    | ChangeOutputFormat ResultsDisplay
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
        (Model Initialising server initialQuery (Ask "ask {?s ?p ?o}") [] Nothing Normal Table Terse [] key, Cmd.none)

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
                newModel = {model | query = query, state = Waiting, queryHistory = query::model.queryHistory}
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
               Table -> ({model | resultsDisplay = Table}, Cmd.none)
               SubjectOrientation -> ({model | resultsDisplay = SubjectOrientation}, Cmd.none)
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
       Nothing -> div [][Html.text "rdfdict erro"]
       Just a ->
            div []
            (List.map (\spo ->
                    div [ class "container"]
                                [ Element.layout [] (viewSubjectMolecule model spo)]
            ) (Dict.values a))

elOfCardAttributes =
    [ Element.Border.rounded 5
    , Element.Border.shadow {offset=(0.0, 4.0), size=8, blur=0, color=Element.rgba 0 0 0 0.2}
    , Element.Background.color colorPalette.material
    , Element.width (Element.px 900)
    ]

elOfSubjectMoleculeCard: Model -> (SubjectMolecule RdfNode) -> Element Msg
elOfSubjectMoleculeCard model mole =
    let
        subj = Tuple.first mole
    in
        Element.column elOfCardAttributes
            [ Element.el [Element.Font.size sizePalette.highlight] 
                (Element.link [] {url=("/index.html?query=describe <"
                ++ (makeRdfKey subj |> Maybe.withDefault "unknown" |> encodeUrlFragmentMarker) 
                ++">"), label=elOfRdfNode model Subject subj}) -- make case here to clean up the view function below
            , elOfPredicates model mole
            ]
 

makeSubjectMoleculeCard: Model -> (SubjectMolecule RdfNode) -> Element Msg
makeSubjectMoleculeCard model mole =    
    let
        subj = Tuple.first mole
    in
        Element.column []
                    [ Element.el [Element.Font.size sizePalette.highlight] 
                        (Element.link [] {url=("/index.html?query=describe <"
                        ++ (makeRdfKey subj |> Maybe.withDefault "unknown" |> encodeUrlFragmentMarker) 
                        ++">"), label=elOfRdfNode model Subject subj}) -- make case here to clean up the view function below
                    , elOfPredicates model mole
                    ]
 
viewSubjectMolecule: Model -> (SubjectMolecule RdfNode) -> Element Msg
viewSubjectMolecule model mole =
    let
        subj = Tuple.first mole
    in
        case subj of
           BlankNode a -> Element.none -- "blank node detected"
           _ -> elOfSubjectMoleculeCard model mole

expandObjectInPlace: Model -> RdfKey -> Html Msg
expandObjectInPlace k dict =
    span [] []

elOfPredicates: Model -> SubjectMolecule RdfNode -> Element Msg
elOfPredicates model mole =
    let
        subj = Tuple.first mole
        preds = Tuple.second mole
    in
        Element.column []
            (List.map(\po -> 
                Element.column []
                    [ elOfRdfNode model Predicate (Tuple.first po)
                    , elOfObjects model subj po
                    ]
            )
            preds)

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
elOfRdfNodeAsPredicate: Model -> RdfNode -> Element Msg
elOfRdfNodeAsPredicate model node =
    case node of
        Uri a ->
            (Element.row [Element.Font.bold][ Element.text (aka model.predicateStyle a.value)
                    , Element.text ": "
                    ])
        _ ->
            Element.el [Element.Font.bold] (Element.text "All predicates should be Uri")


viewRdfNodeAsPredicate: Model -> RdfNode -> Html Msg
viewRdfNodeAsPredicate model node = 
    case node of
        Uri a ->
            b []    [ Html.text (aka model.predicateStyle a.value)
                    , Html.text ": "
                    ]
        _ ->
            b []    [ Html.text "All predicates should be Uri"]

removeUrlFragment: String -> String
removeUrlFragment urlString =
    urlString 
    |> String.split "#"
    |> List.head
    |> Maybe.withDefault urlString 

encodeUrlFragmentMarker: String -> String
encodeUrlFragmentMarker urlString =
    String.replace "#" "%23" urlString

elOfRdfNode: Model -> ViewRdfNodeAs -> RdfNode -> Element Msg
elOfRdfNode model nodeType node =
    case node of
        Uri a ->
            case nodeType of
                Object ->
                    Element.el []   (Element.row [][ Element.link [] { url=("/index.html?query=describe <"++(encodeUrlFragmentMarker a.value)++">")
                                                        , label=Element.text a.value
                                                        }
                                    , Element.newTabLink [] {url=a.value, label = Element.image [] { src = "www-12px.svg", description = "" }}])
                Subject -> Element.text a.value
                Predicate -> 
                    elOfRdfNodeAsPredicate model node --text a.value -- makeSubjectMoleculeCard model node --text a.value  -- a is now an RdfKey and can be expanded via Model
        BlankNode a ->
            case model.currentRdfDict of
               Nothing -> Element.text a.value
               Just dict ->
                    let
                        related = Dict.get a.value dict
                    in
                     case related of
                        Nothing -> Element.text a.value
                        Just subjectMole -> 
--                            Debug.log ("Going after "++a.value)
                            case nodeType of
                                Object ->
                                    (elOfSubjectMoleculeCard model subjectMole) --text a.value  -- a is now an RdfKey and can be expanded via Model
                                Subject -> Element.text a.value
                                Predicate -> elOfRdfNode model Predicate node
        LiteralOnlyValue a ->
            Element.text a.value
        LiteralValueAndDataType a ->
            Element.row [] [ Element.text a.value
                    , Element.row [Element.Font.size sizePalette.smallPrint][ Element.text "  ("
                                                                            , Element.text a.dataType
                                                                            , Element.text ")"]
                                                                            ]
        LiteralValueAndLanguageString a ->
            Element.row [] [ Element.text a.value
                    , Element.row [Element.Font.size sizePalette.smallPrint]
                            [ Element.text "  ("
                            , Element.text a.language
                            , Element.text ")"]]
        Unknown ->
            Element.el [Element.Font.bold] (Element.text "Unrecognised Atom")

viewRdfNode: Model -> ViewRdfNodeAs -> RdfNode -> Html Msg
viewRdfNode model nodeType node = 
    case node of
        Uri a ->
            case nodeType of
                Object ->
                    span [] [ Html.a [href ("/index.html?query=describe <"++(encodeUrlFragmentMarker a.value)++">")][Html.text a.value]  -- remove fragment on anchor
                            , Html.a [href a.value, target "_blank"][img [src "www-12px.svg"][]]
                            ]
                Subject -> span [][Html.text a.value]
                Predicate -> 
                    viewRdfNodeAsPredicate model node --text a.value -- makeSubjectMoleculeCard model node --text a.value  -- a is now an RdfKey and can be expanded via Model
        BlankNode a ->
            case model.currentRdfDict of
               Nothing -> Html.text a.value
               Just dict ->
                    let
                        related = Dict.get a.value dict
                    in
                     case related of
                        Nothing -> span [][Html.text a.value]
                        Just subjectMole -> 
--                            Debug.log ("Going after "++a.value)
                            case nodeType of
                                Object ->
                                    Element.layout [] (elOfSubjectMoleculeCard model subjectMole) --text a.value  -- a is now an RdfKey and can be expanded via Model
                                Subject -> span [][Html.text a.value]
                                Predicate -> viewRdfNode model Predicate node
        LiteralOnlyValue a ->
            Html.text a.value
        LiteralValueAndDataType a ->
            span [] [ Html.text a.value
                    , small [] [Html.text "  (", Html.text a.dataType, Html.text ")"]]
        LiteralValueAndLanguageString a ->
            span [] [ Html.text a.value
                    , small [] [Html.text "  (", Html.text a.language, Html.text ")"]]
        Unknown ->
            b []    [ Html.text "Unrecognised Atom"]

elOfObjects:  Model -> RdfNode -> (RdfNode, List RdfNode) -> Element Msg
elOfObjects model subj po =
    let
        (pred, objs) = po
        head = List.head objs
        rest = List.tail objs
                |> Maybe.withDefault []
        restCount = List.length rest
    in
        case head of
           Just obj -> case restCount of
                                0 -> elOfRdfNode model Object obj
                                _ -> Element.column [] (elOfRestOfObjectList model (subj, pred) obj rest)

           Nothing -> Element.text ""

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

           Nothing -> Html.text ""

elOfRestOfObjectList: Model -> (RdfNode, RdfNode) -> RdfNode -> List RdfNode -> List (Element Msg)
elOfRestOfObjectList model selected obj rest =
    case (List.Extra.find (\o -> o == selected) model.openPredicatesInSubject) of
        Just a -> elOfRdfNode model Object obj
                  :: Element.Input.button []{ onPress=Just (DeregisterSubjectPredicateOpen selected)
                                            , label=Element.text " less"
                                            }
                  :: (List.map (
                        \r -> elOfRdfNode model Object r
                    ) rest)
        Nothing -> [ elOfRdfNode model Object obj
                    , Element.Input.button [] { onPress=Just (RegisterSubjectPredicateOpen selected)
                                        , label=Element.text ((String.fromInt <| List.length rest)++" more")
                                        }
                    ]

viewRestOfObjectList: Model -> (RdfNode, RdfNode) -> RdfNode -> List RdfNode -> List (Html Msg)
viewRestOfObjectList model selected obj rest =
    case (List.Extra.find (\o -> o == selected) model.openPredicatesInSubject) of
        Just a -> viewRdfNode model Object obj
                  :: Html.button [onClick (DeregisterSubjectPredicateOpen selected)] [Html.text " less"]
                  :: (List.map (
                        \r -> div [] [viewRdfNode model Object r]
                    ) rest)
        Nothing -> [ viewRdfNode model Object obj
                    , Html.button [onClick (RegisterSubjectPredicateOpen selected)] [Html.text ((String.fromInt <| List.length rest)++" more")]
                    ]
-- View

makeDict: ServerVars -> List SelectAtom -> Dict String String
makeDict vars atom =
    List.map (\a -> a.value) atom
    |> List.Extra.zip vars
    |> Dict.fromList

queryInput: Server -> SparqlQuery -> Html Msg
queryInput newServer query =
            div [] 
                [ textarea 
                    [ cols 120
                    , rows 15
                    , wrap "soft"
                    , Html.Attributes.placeholder "Sparql Query"
                    , onInput ChangeQuery
                    , value (Sparql.toString query)
                    ][]
                , Html.button [onClick (SubmitQuery query)][Html.text "Submit"]
                ]
-- resultFormatToggle: ResultsDisplay -> Html Msg
-- resultFormatToggle selected = 
--     div []
--         [ Html.text "Output format"
--         , input [ type_ "radio"
--                 , name "resultFormat"
--                 , value Table
--                 , checked (selected == Table)
--                 , onInput ChangeOutputFormat
--                 ][]
--         , Html.text "Table"
--         , input [ type_ "radio"
--                 , name "resultFormat"
--                 , value "subject"
--                 , checked (selected == SubjectOrientation)
--                 , onInput ChangeOutputFormat
--                 ][]
--         , Html.text "Subjects"
--         ]

predicateStyleToggle: PredicateStyle -> Html Msg
predicateStyleToggle selected =
    div []
        [ Html.text "Output format"
        , input [ type_ "radio"
                , name "predicateStyle"
                , value "verbose"
                , checked (selected == Verbose)
                , onInput ChangePredicateStyle
                ][]
        , Html.text "Verbose"
        , input [ type_ "radio"
                , name "predicateStyle"
                , value "terse"
                , checked (selected == Terse)
                , onInput ChangePredicateStyle
                ][]
        , Html.text "Terse"
        ]


uploadQueryFromFile:  Html Msg
uploadQueryFromFile = 
    div []
    [ Html.button [onClick FileRequested][Html.text "Load query"]
    , Html.button [onClick DownloadFile][Html.text "Download query"]]

downloadFile: String -> String -> Cmd msg
downloadFile fileName query =
  Download.string fileName "text" query

--Element Based UI

colorPalette = 
    { header = Element.rgb255 77 195 230
    , background = Element.rgb255 255 247 250 
    , button = Element.rgb255 217 246 255
    , highlight = Element.rgb255 68 242 187
    , material = Element.rgb255 147 214 234
    }

sizePalette = 
    { smallPrint = 8
    , command = 12
    , normal = 14
    , highlight = 20
    }

elOfTabularResults: ServerVars -> ServerForm SelectAtom -> Element msgDecoder
elOfTabularResults vars result = 
    let
        data = List.map (\r -> makeDict vars r) result  -- List dict
        columns = List.map (\v ->   { header = Element.text v
                                    , width = Element.fill
                                    , view = (\x -> Dict.get v x |> Maybe.withDefault "" |> Element.text) 
                                    }) vars
    in
        Element.table   [ Element.Font.size sizePalette.normal
                        , Element.Border.width 2
                        ] {data = data, columns = columns}

elOfQueryHistory: List SparqlQuery -> Element Msg
elOfQueryHistory history =
    Element.textColumn [ spacing 10, padding 10, Element.height (Element.px 120), scrollbarY ]
        ( List.map (\query ->
                Debug.log (Sparql.toString query)
                Element.row [spacing 10][ Element.Input.button
                                                [ Element.Background.color colorPalette.highlight 
                                                , Element.focused
                                                    [ Element.Background.color colorPalette.highlight ]
                                                ]
                                                { onPress = Just (SubmitQueryWhileNavigating query)
                                                , label = Element.text ">"
                                                }
                                        , paragraph [] [ Element.text <| Sparql.toString query]
                                        ]
            ) history)


elOfUploadQueryFromFile: Element Msg
elOfUploadQueryFromFile =
    Element.row [ Element.spacing 5
                , Element.Font.size sizePalette.command
                , Element.padding 10
                , Element.height (Element.px 30)
                , Element.width Element.fill
                , Element.Background.color colorPalette.header
                ]   [ Element.Input.button  [ Element.Background.color colorPalette.button
                                            , Element.height (Element.px 20)
                                            , Element.width (Element.px 100)
                                            ]   { onPress = Just FileRequested
                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Load Query"
                                                }
                    , Element.Input.button  [ Element.Background.color colorPalette.button
                                            , Element.height (Element.px 20)
                                            , Element.width (Element.px 100)
                                            ]   { onPress = Just DownloadFile
                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Download Query"
                                                }
                    ]

elOfResultFormatToggle: Model -> Element Msg
elOfResultFormatToggle model =
    Element.Input.radioRow  [ Element.padding 0
                            , Element.spacing 10
                            , Element.Font.size sizePalette.command
                            ]   { onChange = ChangeOutputFormat
                                , selected = Just model.resultsDisplay
                                , label = Element.Input.labelLeft [Element.Font.size sizePalette.command] (Element.text "Output Format")
                                , options = [ Element.Input.option Table (Element.text "Table")
                                            , Element.Input.option SubjectOrientation (Element.text "Subjects")
                                            ]
                                }

elOfQueryPanel: Model -> Element Msg
elOfQueryPanel model =
    Element.column  [ Element.width Element.fill
                    , Element.spacingXY 0 5
                    ]   [ Element.Input.multiline []
                                { onChange = ChangeQuery
                                , text = (Sparql.toString model.query)
                                , placeholder = Nothing
                                , label = Element.Input.labelLeft [Element.centerY] (Element.none)
                                , spellcheck = False
                                }
                        , Element.row   [Element.paddingXY 8 0]  [ Element.none 
                                                , Element.Input.button  [ Element.width (Element.px 60)
                                                , Element.height (Element.px 30)
                                                , Element.spacingXY 50 0 
                                                , Element.Background.color colorPalette.highlight
                                                ] { onPress = Just (SubmitQuery model.query)
                                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Go!"
                                                                }
                                            ]
                        ]

elOfMainPage: Model -> Element Msg
elOfMainPage model =
    Element.column  [ Element.width Element.fill
                    , Element.Background.color colorPalette.background
                    ]   
                        [ (elOfHeading model)
                        , elOfUploadQueryFromFile
                        , (elOfQueryPanel model)
                        , (elOfResultFormatToggle model)
                        , elOfQueryHistory model.queryHistory
                        ]

elOfHeading: Model -> Element Msg
elOfHeading model =
    Element.el  [ Element.Background.color colorPalette.header
                , Element.height (Element.px 40)
                , Element.width Element.fill
                , Element.Font.size 12
                ] (Element.row  [ Element.centerY
                                , Element.width Element.fill
                                ]   
                                    [ elOfServerInput model
                                    , Element.el [Element.centerY, Element.alignRight, Element.paddingXY 5 0] (Element.text "Sparql Playground - v0.0")
                                    ])

elOfServerInput: Model -> Element Msg
elOfServerInput model = 
    Element.el [] (Element.row  [Element.centerY
                                , Element.spacingXY 5 0
                                ][ Element.Input.text[ Element.width (Element.px 200)
                                                                        , Element.height (Element.px 20)
                                                                        , Element.paddingXY 3 2
                                                                        ]{ onChange = ChangeServer
                                                                            , text = model.server
                                                                            , placeholder = Just (Element.Input.placeholder [Element.centerY] (Element.text "http://your.api.server:port"))
                                                                            , label = Element.Input.labelLeft [Element.centerY] (Element.text "API")
                                                                        }
                                                , Element.Input.button  [ Element.alignLeft
                                                                        , Element.paddingXY 5 0
                                                                        , Element.height (Element.px 20)
                                                                        , Element.Background.color colorPalette.button]   
                                                                            { onPress = Just PingServer
                                                                            , label= (Element.text "Connect")
                                                                            }
                                                ])


view: Model -> Document Msg
view model = { title = "Sparql Query Playground - 0.0"
            , body = (List.singleton <|
                    case model.state of
                        Initialising ->
                                Element.layout []  (elOfHeading model)-- h1 [][Html.text "Sparql Query Playground v0.1"]
                                -- , div [] 
                                --     [ input [Html.Attributes.placeholder "Server", onInput ChangeServer, value model.server][]
                                --     , Html.button [onClick PingServer][Html.text "Connect"]
                                --     ]
                        Pinging -> 
                            div [][Html.text <| "Pinging"++model.server]
                        Querying -> 
                                Element.layout [] (elOfMainPage model)
--                                , Element.layout [] (elOfQueryHistory model.queryHistory)
--                                , resultFormatToggle model.resultsDisplay
                        Waiting -> div [style "cursor" "progress"]
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , b [] [Html.text "Wating for server response..."]
                                ]
                        ApiError error -> 
                            case error of
                                Http.BadBody err ->
                                    div []
                                        [ h1 [][Html.text "ApiError: I can't interpret the response"]
                                        , div [][Html.text err]
                                        , Html.button [onClick BackToQuery][Html.text "Back"]
                                        ]
                                _ ->
                                    div [][
                                        h1 [][Html.text "ApiError: Oops - something went wrong! :-("]
                                        , Html.button [onClick BackToQuery][Html.text "Back"]
                                    ]
                        DisplayingSelectError message ->
                            div []                
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , div [][Html.text message]
                                ]
                        DisplayingSelectResult vars result ->
                            case model.resultsDisplay of
                                Table ->
                                        Element.column  [ Element.width Element.fill] [ elOfMainPage model
                                                        , elOfTabularResults vars result
                                                        ]
                                        |> Element.layout []
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
                                                , Element.layout [] (elOfQueryHistory model.queryHistory)
--                                                , resultFormatToggle model.resultsDisplay
                                                , h2 [][Html.text "Subject orientation"]
                                                , predicateStyleToggle model.predicateStyle
                                                , br [] [] 
                                                -- ??? replace below by passing the whole model in
                                                , viewSubjects model
                                                , hr [] []
                                                , div [][Html.text "Graph nav (off)"]
                                                ]
                                        Nothing ->                             
                                            div []                
                                                [ uploadQueryFromFile
                                                , queryInput model.server model.query
--                                                , resultFormatToggle model.resultsDisplay
                                                , h4 [][Html.text "Subject orientation only where results are in the shape of ?s ?p ?o"]
                                                ]
            )}

-- Decoders

msgDecoder : Decoder Msg
msgDecoder =
    field "key" string
        |> Json.Decode.map PressedKey

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
