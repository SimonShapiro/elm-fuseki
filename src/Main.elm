port module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, h2, h4, span, ul, li, b, p, hr, br, table, tr, th, td, small)
import Html exposing (textarea, a, img)
import Html.Attributes exposing (href, placeholder, value, class, rows, cols, wrap, style, type_, name, checked, src, target)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation exposing (pushUrl, replaceUrl, load, Key)
import Http
import Html.Events exposing (on)
import Json.Decode as JD exposing (Decoder, Error, errorToString, field, string, map, map2, map3, map4, map5, map6, map7, list, int, decodeString, at, andThen)
import Json.Encode as JE exposing (..)
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

import Graph exposing (..)
import Graph.DOT exposing (..)
import Json.Decode exposing (index)
import String

import Sparql exposing (..)
import PlaygroundQuery exposing (..)
import RdfDict exposing (..)
import DisplayGraph exposing (..)
import List.Extra exposing (uncons, groupWhile)

import Element exposing (..)
import Element.Border exposing (..)
import Element.Background exposing (..)
import Element.Input exposing (..)
import Element.Font exposing (..)
import Element.Region exposing (..)
import Element.Events exposing (..)
import Dict
import List.Extra
import Element
import String.Extra
import Markdown.Block as Block exposing (Block, Inline, ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
import Element
import RdfDict

import Dagre 
import Element

import Draggable
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import TypedSvg exposing (svg)
import TypedSvg.Attributes as Attr exposing (width, height) 
-- (x, y, cx, cy, fill, r, stroke, strokeWidth, viewBox, x, y, width, height, title, points, orient)
import TypedSvg.Types exposing (Paint(..), px)
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import String

dagreOptions = 
    { rankDir = Dagre.TB
    , align = Nothing
    }

emptyDagre = Dagre.UnplacedDagreGraph dagreOptions [] []

convertCommunityGraphToDagreWithoutLayout : Graph  (SubjectMolecule RdfNode) String -> (List Dagre.UnplacedNode, List Dagre.UnplacedEdge)
convertCommunityGraphToDagreWithoutLayout g = 
    let
        edges = List.map (\e -> {from = e.from, to=e.to, label=e.label, width = 60, height = 15}) (Graph.edges g)
        nodes = List.map (\n ->     { id = n.id
                                    , label= RdfDict.rdfNodeToMaybeString (Tuple.first n.label) |> Maybe.withDefault "unknown"
                                    , width = 120
                                    , height = 40
                                     }) (Graph.nodes g)
    in
        (nodes, edges)
convertyGraphToDagreWithoutLayout : Dagre.GraphOptions -> Graph  (String, String) (String, String) -> Dagre.UnplacedDagreGraph
convertyGraphToDagreWithoutLayout options g = 
    let
        edges = List.map (\e -> {from = e.from, to=e.to, label=Tuple.first e.label, width = 60, height = 15}) (Graph.edges g)
        nodes = List.map (\n ->     { id = n.id
                                    , label= Tuple.first n.label
                                    , width = 120
                                    , height = 40
                                     }) (Graph.nodes g)
    in
        { options=options
        , nodes=nodes
        , edges=edges}

unzip3 list =
  let (a, b, c ) = unzip3Help list
  in ( List.reverse a, List.reverse b, List.reverse c )
unzip3Help list =
  List.foldl
    (\(a, b, c) (as_, bs, cs) -> ( a :: as_, b :: bs, c :: cs ))
    ([],[],[])
    list

selectAtomAsTuple : SelectAtom -> (String, String)
selectAtomAsTuple atom =
    (atom.value, atom.aType)

convertServerFormToCommunityGraph : ServerForm SelectAtom -> Graph (String, String) (String, String)
convertServerFormToCommunityGraph spo =
    let
        triples = makeTripleForm spo
--        List(a, b, c) -> (List a, List b, List c)
        (subjects, predicates, objects) = case triples of
                                            Just t -> t |> unzip3
                                            Nothing -> ([], [], [])  
        --extracted = List.map()
        comparableSubjects = List.map (\s -> selectAtomAsTuple s) subjects
--        comparablePredicates = List.map (\s -> selectAtomAsTuple s) predicates
        comparableObjects = List.map (\s -> selectAtomAsTuple s) objects
        combinedSubjectObjects = List.append comparableSubjects comparableObjects
                                |> List.Extra.unique
        reverseDict = List.indexedMap Tuple.pair combinedSubjectObjects
                        |> List.map(\(x, y) -> (y, x))
                        |> Dict.fromList
        nodes = List.indexedMap Tuple.pair combinedSubjectObjects
                |> List.map (\(x, y) -> Graph.Node x y)

        edges = case triples of 
            Just t -> List.map (\triple -> 
                                    let
                                        (s, p, o) = triple 
                                        sindex = Dict.get (selectAtomAsTuple s) reverseDict |> Maybe.withDefault -1
                                        oindex = Dict.get (selectAtomAsTuple o) reverseDict |> Maybe.withDefault -1
                                    in
                                        Graph.Edge sindex oindex (selectAtomAsTuple p)
                                ) t
            Nothing -> []
    in
        Graph.fromNodesAndEdges nodes edges


convertDagreWithoutLayoutToJson : Dagre.UnplacedDagreGraph -> JE.Value
convertDagreWithoutLayoutToJson dagre = 
    let
        jsonOptions = JE.object [ ("orientation", JE.string (Dagre.rankDirToString dagre.options.rankDir)

                                )]
        jsonNodes = JE.list (\n -> JE.object    [ ("id", JE.int n.id)
                                                , ("label", JE.string n.label) 
                                                , ("width", JE.int n.width)
                                                , ("height", JE.int n.height)
                                                ]) dagre.nodes
        jsonEdges = JE.list (\e -> JE.object    [ ("from", JE.int e.from)
                                                , ("to", JE.int e.to)
                                                , ("label", JE.string e.label)
                                                , ("width", JE.int e.width)
                                                , ("height", JE.int e.height)
                                                ]) dagre.edges
    in
        JE.object   [ ("action", JE.string "LayoutGraph")
                    , ("options", jsonOptions)
                    , ("nodes", jsonNodes)
                    , ("edges", jsonEdges)
                    ]

--rdfDictToJsonValue = convertRdfDict2CommunityGraph >> convertCommunityGraphToDagreWithoutLayout >> convertDagreWithoutLayoutToJson
resultsToJsonValue = convertServerFormToCommunityGraph 
                        >> convertyGraphToDagreWithoutLayout Dagre.defaultG
                        >> convertDagreWithoutLayoutToJson

version : String
version = "v0.1"
type alias Document msg =
    { title : String
    , body : List (Html msg)
    }

type Cardinality 
    = OneToOne 
    | ZeroOrOne 
    | ZeroToMany 
    | OneToMany 
type alias Size num =
    { width : num
    , height : num
    }

type UIState 
    = Initialising 
    | Pinging 
    | Querying 
    | DisplayingSelectResult  ServerVars (ServerForm SelectAtom)
    | DisplayingSelectError String
    | ApiError Http.Error
    | Waiting

type GraphDisplayState
    = Unavailable
    | Requested
    | Available Dagre.PlacedGraph

type alias Model = 
    { state: UIState
    , server: Server
    , urlQuery: Maybe SparqlQuery
    , query: SparqlQuery
    , queryHistory: List SparqlQuery
    , resultHistory: Dict String (ServerVars, ServerForm SelectAtom) -- key is the String of the Query
    , lineOfThought: List SparqlQuery
    , vars: ServerVars
    , results: ServerForm SelectAtom
    , currentRdfDict: Maybe RdfDict
    , keyboard: KeyboardMode
    , resultsDisplay: ResultsDisplay
    , predicateStyle: PredicateStyle
    , graphDisplay: GraphDisplay
    , graphImage: GraphDisplayState
    , openPredicatesInSubject: OpenPredicatesInSubject
    , key: Key
    , zoom : Float
    , center : Vec2
    , size : Size Float
    , drag : Draggable.State ()
    }

type alias OpenPredicatesInSubject = List (RdfNode, RdfNode)

type KeyboardMode
    = Normal
    | ReadyToAcceptControl

type GraphDisplay
    = On
    | Off

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
    | DownloadResultsAsCSV
    | ChangeOutputFormat ResultsDisplay
    | ChangePredicateStyle PredicateStyle
    | BackToQuery
    | RegisterSubjectPredicateOpen (RdfNode, RdfNode)
    | DeregisterSubjectPredicateOpen (RdfNode, RdfNode)
    | ClickedLink UrlRequest
    | AddQueryToLineOfThought
    | DisplayFromLineOfThought SparqlQuery
    | ResetLineOfThought
    | ClearCaches
    | ToggleGraph GraphDisplay
    | ReceivedMessageFromWorker Value
    | DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | Zoom Float

type alias KGResponse =  -- a copy of the query is available in the api
    { server: Server
    , status: Int
    , message: String
    , queryType: String
    , query: String
    , vars: ServerVars
    , result: ServerForm SelectAtom
    }

port setStorage : Value -> Cmd msg
port sendActionRequestToWorker : Value -> Cmd msg
port receiveActionResultFromWorker : (Value -> msg) -> Sub msg

renderer : Markdown.Renderer.Renderer (Element msg)
renderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = \value -> Element.paragraph [] [ Element.text value ]
    , strong = \content -> Element.paragraph [ Element.Font.bold ] content
    , emphasis = \content -> Element.paragraph [ Element.Font.italic ] content
    , strikethrough = \content -> Element.paragraph [ Element.Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink []
                { url = destination
                , label =
                    Element.paragraph
                        [ Element.Font.color (Element.rgb255 0 0 255)
                        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
                        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.paragraph
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.paragraph [ Element.spacing 5 ]
                                [ Element.paragraph [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , table = Element.column []
    , tableHeader =
        Element.column
            [ Element.Font.bold
            , Element.width Element.fill
            , Element.Font.center
            ]
    , tableBody = Element.column []
    , tableRow = Element.row [ Element.height Element.fill, Element.width Element.fill ]
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph
                tableBorder
                children
    , html = Markdown.Html.oneOf []
    }


heading : { level : Block.HeadingLevel, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Element.Font.size
            (case level of
                Block.H1 ->
                    36

                Block.H2 ->
                    24

                _ ->
                    20
            )
        , Element.Font.bold
        , Element.Font.family [ Element.Font.typeface "Montserrat" ]
        , Element.Region.heading (Block.headingLevelToInt level)
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.paragraph
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.htmlAttribute (Html.Attributes.style "overflow-wrap" "break-word")
        , Element.htmlAttribute (Html.Attributes.style "word-break" "break-word")
        , Element.padding 20
        , Element.Font.family
            [ Element.Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        [ Element.text details.body ]

tableBorder =
    [ Element.Border.color (Element.rgb255 223 226 229)
    , Element.Border.width 1
    , Element.Border.solid
    , Element.paddingXY 6 13
    , Element.height Element.fill
    ]

rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.split " "
        |> String.join "-"
        |> String.toLower


elOfMarkdown : String -> Result String (List (Element msg))
elOfMarkdown markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Renderer.render renderer)

convertRdfDict2CommunityGraph : RdfDict -> Graph.Graph (SubjectMolecule RdfNode) String 
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
-- Debug.log ("Inside conversion"++(List.length values |> String.fromInt))
-- Debug.log ("Building graph of "++(List.length nodes |> String.fromInt)++":"++(List.length edges |> String.fromInt))
        Graph.fromNodesAndEdges nodes edges


server : Server
server = "http://localhost:port"

-- startWith: Model
-- startWith = Model Initialising server "" Normal Table Terse [] 


initialFn : (Maybe String ) -> Url -> Key -> (Model, (Cmd Msg))
initialFn maybeServer url elmKey =
    let
        serverFlag = case maybeServer of
                        Nothing -> server
                        Just s -> s
        _ = Debug.log ("Received from browser"++serverFlag)
        initialQuery = parseUrlForIndexQuery url
        initialModel : Model
        initialModel =  { state = Initialising
                        , server = serverFlag
                        , urlQuery = initialQuery
                        , query =  (Ask "ask {?s ?p ?o}")
                        , queryHistory = []
                        , vars = []
                        , results = []
                        , resultHistory = Dict.empty
                        , lineOfThought = []
                        , currentRdfDict = Nothing
                        , keyboard = Normal
                        , resultsDisplay = Table
                        , predicateStyle = Terse
                        , graphDisplay = Off
                        , graphImage = Unavailable
                        , openPredicatesInSubject = []
                        , key = elmKey
                        , zoom = 1
                        , center = Vector2.vec2 0 0
                        , size = Size 1400 700 -- ??? what is the purpose of size here
                        , drag = Draggable.init
                        }
    in
        (initialModel, Cmd.none)

parseUrlForDetailsSubject : Url -> Maybe String
parseUrlForDetailsSubject url =
    let
        subject : Query.Parser (Maybe String)
        subject = 
            Query.string "subject"
        parseQuery =
            (s "details" <?> subject)
    in
        Url.Parser.parse parseQuery url |> join

parseUrlForIndexQuery : Url -> Maybe SparqlQuery
parseUrlForIndexQuery url =
    let
        subject : Query.Parser (Maybe String)
        subject = 
            Query.string "query"
        parseQuery =
            (s "index.html" <?> subject)
    in
        Url.Parser.parse parseQuery url |> join |> Maybe.map establishQueryType

-- Utility functions

urlTextAbbreviator : String -> String
urlTextAbbreviator longUrl = 
-- the size here is dependent on font choosen, so shouldn't be constants
    let
        halfLength = String.length longUrl // 2 |> min 40
        usableString = if (String.length longUrl) > 86
            then
                (String.left halfLength longUrl)++"..."++(String.right halfLength longUrl)
            else
                longUrl
    in
        usableString

prefixHumnaReadablePartOfUrl : String -> String -> String
prefixHumnaReadablePartOfUrl prefix urlString = 
    let
        lastPart = urlString 
                    |> String.split "/"
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""
        firstPart = urlString
                    |> String.split "/"
                    |> List.drop -1
        prefixed = [prefix++lastPart]
        reconstructedUrl = List.append firstPart prefixed
                            |> String.join ""
    in
        reconstructedUrl

dragConfig : Draggable.Config () Msg
dragConfig =
    Draggable.basicConfig (OnDragBy << (\( dx, dy ) -> Vector2.vec2 dx dy))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
        OnDragBy rawDelta ->
            let
                delta =
                    rawDelta
                    --    |> Vector2.scale (-1 / model.zoom)
                        |> Vector2.scale (-1 / model.zoom)
            in
            ( { model | center = model.center |> Vector2.add delta }, Cmd.none )

        Zoom factor ->
            let
                newZoom =
                    model.zoom
                        |> (+) (factor * 0.05)
                        |> clamp 0.5 5
            in
            Debug.log ("New zoom"++(String.fromFloat newZoom))
            ( { model | zoom = newZoom }, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        ClickedLink urlRequest ->
            case urlRequest of
                Internal url ->
                    case parseUrlForIndexQuery url of
                        Nothing ->
-- Debug.log ("Internal update running on NOTHING")    --(Url.toString url)) 
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
        ChangeServer newServer -> ({model | server = newServer}, Cmd.none)
        PingServer -> ({model | state = Pinging}, pingServer model.server)
        Pinged result ->
            case result of
                Ok _ -> 
                    case model.urlQuery of
                        Nothing ->
-- Debug.log "Pinged OK - no initial query"
                            ({model | state = Querying, keyboard = ReadyToAcceptControl}, setStorage (JE.string model.server))
                        Just query -> 
                            let
                                command = submitQuery model.server query (Http.expectJson GotSparqlResponse mainDecoder)
                                _ = Debug.log "Pinged OK with" query

                            in
                                ({ model | state = Querying, keyboard = ReadyToAcceptControl, query = query}, setStorage (JE.string model.server))
                Err e -> 
-- Debug.log "Pinged ERROR"
                    ({model | state = Initialising}, Cmd.none)
        ChangeQuery newQuery -> 
-- Debug.log ("Query "++newQuery)
                ({model | query = (establishQueryType newQuery)}, Cmd.none)
        SubmitQuery query -> 
            case model.query of
                        Unrecognised newQuery -> ({model | state = ApiError (Http.BadBody ("I don't recognise this query type "++(Sparql.toString model.query)))}, Cmd.none)
                        _ ->
                            let
                                cachedResult = Dict.get  (Sparql.toString query) model.resultHistory
                            in
                            --    Debug.log ("Submitting Query " model.query
                                case cachedResult of
                                    Nothing ->
-- Debug.log "Not from cache :-("
                                        ({model | state = Waiting}, pushUrl model.key (relative [][Url.Builder.string "query" (Sparql.toString query)])) -- submitQuery model.server query)
                                    Just result -> 
                                        let
                                            vars = Tuple.first result
                                            table = Tuple.second result
                                        in
                                        
-- Debug.log "Cached :-)"
                                        ({ model | state = DisplayingSelectResult vars table
                                        , vars = vars
                                        , results = table
                                        , currentRdfDict = contractResult vars table 
                                                            |> Maybe.map makeRdfDict 
                                        }, Cmd.none)
        SubmitQueryWhileNavigating query ->
            let
                newModel = {model | query = query, state = Waiting, queryHistory = query::model.queryHistory}
                _ = Debug.log "query=" newModel.query
            in
                (newModel, submitQuery newModel.server query (Http.expectJson GotSparqlResponse mainDecoder))         
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    case okData.status of
                        200 ->
                            -- could transform to graph here
                            let
                                resultHistory = case establishQueryType okData.query of
                                                    Load _ -> Dict.empty
                                                    Insert _ -> Dict.empty
                                                    Drop _ -> Dict.empty
                                                    _ -> Dict.insert okData.query (okData.vars, okData.result) model.resultHistory
                                lineOfThought = case establishQueryType okData.query of
                                                    Load _ -> []
                                                    Insert _ -> []
                                                    Drop _ -> []
                                                    _ -> model.lineOfThought
                                contracted = contractResult okData.vars okData.result  -- Maybe (ContractedForm SelectAtom)
                                                        |> Maybe.map makeRdfDict 
                                -- cmd = case contracted of
                                --     Just a -> rdfDictToJsonValue a |> sendActionRequestToWorker
                                --     _ -> Cmd.none
                                cmd = resultsToJsonValue okData.result |> sendActionRequestToWorker
                            in
                                    ({ model | state = DisplayingSelectResult okData.vars okData.result
                                    , vars = okData.vars
                                    , results = okData.result
                                    , resultHistory = resultHistory
                                    , lineOfThought = lineOfThought
                                    , currentRdfDict = contracted
                                    , graphImage = Requested
                                    }, cmd)
                        _ ->
                            ({model | state = DisplayingSelectError okData.message}, Cmd.none)
                Err e -> 
-- Debug.log "Response ERROR"
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
        DownloadResultsAsCSV  -> 
            let
                headedResults = (String.join "|" model.vars) :: List.map (\r -> extractValues r
                                                        |> String.join "|"
                                                 ) model.results
            in
                (model, downloadFile "result.csv" <| String.join "\n" headedResults)
        ChangeOutputFormat outputFormat ->
            case outputFormat of
               Table -> ({model | resultsDisplay = Table}, Cmd.none)
               SubjectOrientation -> ({model | resultsDisplay = SubjectOrientation}, Cmd.none)
        ChangePredicateStyle predicateStyle ->
            case predicateStyle of
               Verbose -> ({model | predicateStyle = Verbose}, Cmd.none)
               Terse -> ({model | predicateStyle = Terse}, Cmd.none)
        ToggleGraph graphDisplay ->
            case graphDisplay of
               On -> ({model | graphDisplay = On}, Cmd.none)
            -- On -> ({model | graphDisplay = On}, )
               Off -> ({model | graphDisplay = Off}, Cmd.none)
        RegisterSubjectPredicateOpen selected -> 
            ({model | openPredicatesInSubject = selected::model.openPredicatesInSubject}, Cmd.none)
        DeregisterSubjectPredicateOpen selected -> 
            ({model | openPredicatesInSubject = List.Extra.remove selected model.openPredicatesInSubject}, Cmd.none)
        AddQueryToLineOfThought -> 
            ({model | lineOfThought = model.query::model.lineOfThought}, Cmd.none)
        DisplayFromLineOfThought tQuery ->
            let 
                thought = Dict.get (Sparql.toString tQuery) model.resultHistory |> Maybe.withDefault ([],[])
                (vars, table) = thought
                cmd = resultsToJsonValue table |> sendActionRequestToWorker
            in
                ({ model | state = DisplayingSelectResult vars table
                , query = tQuery
                , vars = vars
                , results = table
                , currentRdfDict = contractResult vars table 
                                    |> Maybe.map makeRdfDict 
                }, cmd)
        ResetLineOfThought -> 
            ({ model | lineOfThought = []}, Cmd.none)
        ClearCaches -> 
            ({ model | lineOfThought = [], resultHistory = Dict.empty}, Cmd.none)
        ReceivedMessageFromWorker message ->
            let
--                _ = Debug.log ("Received this message from the worker "++message)
                graphDecorder = JD.map2 (\a b -> {width = a, height = b})
                        (at ["graph", "width"] JD.float) 
                        (at ["graph", "height"] JD.float)
                nodeDecoder = JD.map6 Dagre.PlacedNode
                                    (JD.field "id" JD.int)
                                    (JD.field "label" JD.string)
                                    (JD.field "width" JD.int)
                                    (JD.field "height" JD.int)
                                    (JD.field "x" JD.float)
                                    (JD.field "y" JD.float)
                                    |> JD.list

                pointDecoder = JD.map2 Dagre.Point
                                    (JD.field "x" JD.float)   
                                    (JD.field "y" JD.float)
                                    |> JD.list 

                edgeDecoder = JD.map8 Dagre.PlacedEdge
                                    (JD.field "from" JD.int)
                                    (JD.field "to" JD.int)
                                    (JD.field "label" JD.string)
                                    (JD.field "width" JD.int)
                                    (JD.field "height" JD.int)
                                    (JD.field "x" JD.float)
                                    (JD.field "y" JD.float)
                                    (JD.field "points" pointDecoder)
                                    |> JD.list

                mNodeList = JD.field "nodes" nodeDecoder
                mGraph = JD.decodeValue 
                            (JD.map3 (\a b c -> {graph = a, nodes = b, edges = c})
                                graphDecorder
                                mNodeList
                                (JD.field "edges" edgeDecoder))
                                message

            in
                case mGraph of 
                    Err _ -> 
-- Debug.log "Decode Error"
                        ({model | graphImage = Unavailable}, Cmd.none)
                    Ok a -> 
                        let
                            zoom = (min (model.size.width/a.graph.width) (model.size.height/a.graph.height))
                            --center = Vector2.vec2 (a.graph.width/2) (a.graph.height/2)
 --                           center = Vector2.vec2 (a.graph.width/2) (a.graph.height/2)
                        in
                            Debug.log ("Setting zoom to "++(String.fromFloat <| min (model.size.width/a.graph.width) (model.size.height/a.graph.height)))
                            Debug.log ("Setting width to"++(String.fromFloat <| a.graph.width))
                            Debug.log ("Setting height to"++(String.fromFloat <| a.graph.height))
                            ({model | graphImage = Available a
   --                                 , center = center
                                    , zoom = zoom
                                    }, Cmd.none)

handleUrlRequest : UrlRequest -> Msg
handleUrlRequest req = 
    case req of
        Internal url ->
-- Debug.log ("Handling internal request to "++(Url.toString url))
            ClickedLink req
        External url ->
-- Debug.log ("Handling external request to "++url)
            ClickedLink req


handleUrlChange : Url -> Msg
handleUrlChange url = 
    case (parseUrlForIndexQuery url) of -- have to reparse url
        Nothing ->
-- Debug.log ("fwd/bck update running on NOTHING")    --(Url.toString url)) 
            NoOp
        Just a -> 
            let
                _ = Debug.log "fwd/bck update running " a    --(Url.toString url)) 
            in
                SubmitQueryWhileNavigating  a

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch   [ receiveActionResultFromWorker ReceivedMessageFromWorker
                , Draggable.subscriptions DragMsg model.drag
    ]

pingServer : Server -> (Cmd Msg)
pingServer newServer = 
  --  let
-- Debug.log ("Pinging"++newServer)

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

aka : PredicateStyle -> String -> String
aka predicateStyle pred = 
    case predicateStyle of
       Verbose -> pred
       Terse -> String.split "/" pred
                |> List.reverse
                |> List.head
                |> Maybe.withDefault pred

elOfLineOfThought : Model -> Element Msg
elOfLineOfThought model =
    Element.row [] (List.append ((Element.Input.button  
                                    [ Element.alignLeft
                                    , Element.Background.color colorPalette.button
                                    , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                    , Element.width (Element.px 30)
                                    ]
                                    { onPress=Just AddQueryToLineOfThought
                                    , label=Element.image [] {src = "red_pin.svg", description="Red Pin"}
                                    })::(List.indexedMap (\ndx tQuery -> 
                                    Element.row [] 
                                    [ Element.text "---"
                                    , Element.Input.button 
                                                    [ Element.Background.color colorPalette.button
                                                    , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                                    ]  
                                                    { onPress=Just (DisplayFromLineOfThought tQuery)
                                                    , label=Element.text ((String.fromInt <| (List.length model.lineOfThought) - ndx))
                                                    }
                                    ]) model.lineOfThought |> List.reverse))
                                    [ Element.text "-->"
                                    , Element.Input.button 
                                                    [ Element.Background.color colorPalette.button
                                                    , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                                    ]  
                                                    { onPress=Just ResetLineOfThought
                                                    , label=Element.text " Reset"
                                                    }

        ])
    

elOfSubjects : Model -> Element Msg 
elOfSubjects model =
    case model.currentRdfDict of
       Nothing -> Element.text "rdfdict error"
       Just a ->
            Element.column [ Element.spacingXY 0 25
                            , Element.padding 15
                            ]
                        ((List.map (\spo ->
                            viewSubjectMolecule model spo)
                        ) (Dict.values a))

elOfCardAttributes : List (Element.Attribute msg)
elOfCardAttributes =
    [ Element.Border.rounded 5
    , Element.Border.width 1    
    , Element.Border.shadow {offset=(0.0, 4.0), size=4, blur=0, color=Element.rgba 0 0 0 0.1}
    , Element.Background.color colorPalette.material
    , Element.width (Element.px 900)
    , Element.paddingXY 5 0
    ]

elOfSubjectMoleculeCard : Model -> (SubjectMolecule RdfNode) -> Element Msg
elOfSubjectMoleculeCard model mole =
    let
        subj = Tuple.first mole
        backLinksQuery target =  String.join "\n" 
            [ 
            --    "base <http://kg.info/temp/> , 
            "construct"
            , "{<"++target++"> ?passive ?s } {"
            , "?s ?p <"++target++">. "
            , "bind(uri(concat(str(?p), '_Inverted')) as ?passive)}"        
            ]
    in
        case subj of
            BlankNode _ -> 
                Element.column [Element.paddingXY 10 0, Element.Border.dotted, Element.Border.width 1]
                    [ elOfPredicates model mole
                    ]
            
            Uri _ ->
                Element.column elOfCardAttributes
                    [ Element.el [Element.Font.size sizePalette.command, Element.alignRight]
                        (Element.row [][ 
                            (Element.link [] {url=("/index.html?query="
                            ++ (makeRdfKey subj |> Maybe.withDefault "unknown" |> encodeUrlFragmentMarker |> backLinksQuery) 
                            ), label=Element.text "Back links"})
                        ])
                    , Element.el [Element.Font.size sizePalette.subject] 
                        (Element.link [] {url=("/index.html?query=describe <"
                        ++ (makeRdfKey subj |> Maybe.withDefault "unknown" |> encodeUrlFragmentMarker) 
                        ++">"), label=elOfRdfNode model Subject subj}) -- make case here to clean up the view function below
                    , elOfPredicates model mole
                    ]
            _ -> Element.none

viewSubjectMolecule : Model -> (SubjectMolecule RdfNode) -> Element Msg
viewSubjectMolecule model mole =
    let
        subj = Tuple.first mole
    in
        case subj of
           BlankNode a -> Element.none -- "blank node detected"
           _ -> elOfSubjectMoleculeCard model mole

elOfRdfNodeValue : RdfNode -> Element Msg
elOfRdfNodeValue node =
    let
        leadingWhiteSpaceRe = Maybe.withDefault Regex.never <| Regex.fromStringWith { caseInsensitive = True, multiline = True } "^[ ,\t]+"
    in
        case node of 
            LiteralOnlyValue a -> Element.text a.value
            LiteralValueAndLanguageString a -> Element.text a.value
            LiteralValueAndDataType a -> 
                if (a.dataType == "http://net.daringfireball.markdown") 
                then   
                    case (Regex.replace leadingWhiteSpaceRe (\_ -> "") a.value |> elOfMarkdown) of
                        Ok rendered -> Element.column
                                        [ Element.spacing 30
                                        , Element.padding 50
                                        ]
                                        rendered

                        Err errors ->
                                    Element.text errors
                else Element.text a.value
            _ -> Element.none

elOfPredicates : Model -> SubjectMolecule RdfNode -> Element Msg
elOfPredicates model mole =
    let
        subj = Tuple.first mole
        preds = Tuple.second mole
    in
        Element.column [Element.spacingXY 0 2]
            (List.map(\po -> 
                Element.column [Element.spacingXY 0 0]   -- attempt to switch off spacingXY above - not working
                    [ elOfRdfNode model Predicate (Tuple.first po)
                    , elOfObjects model subj po
                    ]
            )
            preds)

elOfRdfNodeAsPredicate : Model -> RdfNode -> Element Msg
elOfRdfNodeAsPredicate model node =
    case node of
        Uri a ->
            (Element.row    [ Element.Font.bold
                            , Element.Font.size sizePalette.command
                            ]
                            [ Element.text (aka model.predicateStyle a.value)
                    , Element.text ": "
                    ])
        _ ->
            Element.el  [ Element.Font.bold
                        , Element.Font.size sizePalette.command
                        ] (Element.text "All predicates should be Uri")

encodeUrlFragmentMarker : String -> String
encodeUrlFragmentMarker urlString =
    String.replace "#" "%23" urlString

elOfRdfNode : Model -> ViewRdfNodeAs -> RdfNode -> Element Msg
elOfRdfNode model nodeType node =
    case node of
        Uri a ->
            case nodeType of
                Object ->
                    Element.el [Element.paddingXY 5 0, Element.Font.size sizePalette.normal]   (Element.row []
                                        [ Element.link [ Element.Font.color colorPalette.header
                                        , Element.mouseOver [Element.Font.color colorPalette.lowlight]
                                        ] 
                                        { url=("/index.html?query=describe <"++(encodeUrlFragmentMarker a.value)++">")
                                        , label=Element.text <| aka model.predicateStyle <| urlTextAbbreviator <| a.value  
                                        }
                                    , Element.newTabLink [] {url=a.value, label = Element.image [] { src = "www-12px.svg", description = "" }}])
                Subject -> Element.text (aka model.predicateStyle a.value) -- a.value
                Predicate -> 
                    elOfRdfNodeAsPredicate model node --text a.value -- makeSubjectMoleculeCard model node --text a.value  -- a is now an RdfKey and can be expanded via Model
        BlankNode a ->
            case model.currentRdfDict of
               Nothing -> 
                    Element.text a.value
               Just dict ->
                    let
                        related = Dict.get a.value dict
                    in
                     case related of
                        Nothing -> 
                            Element.text (a.value++"bnode") -- create a decent contruct query here.  By the time you are here its too late :-(
                        Just subjectMole -> 
--                            Debug.log ("Going after "++a.value)
                            case nodeType of
                                Object ->
                                    (elOfSubjectMoleculeCard model subjectMole) --text a.value  -- a is now an RdfKey and can be expanded via Model
                                Subject -> 
                                    Element.text a.value
                                Predicate -> 
                                        elOfRdfNode model Predicate node
        LiteralOnlyValue a ->
-- Debug.log ("Hunting the string wrapping issue "++(String.left 20 a.value)++(a.value |> String.words |> List.length |> String.fromInt))
            Element.paragraph [Element.paddingXY 5 0, Element.Font.size sizePalette.normal, Element.width fill] [Element.text a.value]
        LiteralValueAndDataType a ->
            Element.paragraph [Element.paddingXY 5 0, Element.Font.size sizePalette.normal, Element.width fill] [ elOfRdfNodeValue node
                                , Element.row [Element.Font.size sizePalette.smallPrint][ Element.text "  ("
                                                , Element.text a.dataType
                                                , Element.text ")"]
                                ]

        LiteralValueAndLanguageString a ->
            Element.paragraph [Element.paddingXY 5 0, Element.Font.size sizePalette.normal] [ Element.text a.value
                    , Element.row [Element.Font.size sizePalette.smallPrint]
                            [ Element.text "  ("
                            , Element.text a.language
                            , Element.text ")"]]
        Unknown ->
            Element.el [Element.Font.bold] (Element.text "Unrecognised Atom")

elOfObjects :  Model -> RdfNode -> (RdfNode, List RdfNode) -> Element Msg
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

elOfRestOfObjectList : Model -> (RdfNode, RdfNode) -> RdfNode -> List RdfNode -> List (Element Msg)
elOfRestOfObjectList model selected obj rest =
    case (List.Extra.find (\o -> o == selected) model.openPredicatesInSubject) of
        Just _ -> Element.Input.button   [ Element.alignLeft
                                            , Element.Background.color colorPalette.button
                                            , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                            , Element.Font.size sizePalette.command
                                            ]
                                            { onPress=Just (DeregisterSubjectPredicateOpen selected)
                                            , label=Element.text " less"
                                            }
                  :: elOfRdfNode model Object obj
                  :: (List.map (
                        \r -> elOfRdfNode model Object r
                    ) rest)
        Nothing -> [ elOfRdfNode model Object obj
                    , Element.Input.button [ Element.alignLeft
                                            , Element.Background.color colorPalette.button
                                            , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                            , Element.Font.size sizePalette.command
                                            ] 
                                            { onPress=Just (RegisterSubjectPredicateOpen selected)
                                        , label=Element.text ((String.fromInt <| List.length rest)++" more")
                                        }
                    ]
-- View

makeDict : ServerVars -> List SelectAtom -> Dict String String
makeDict vars atom =
    List.map (\a -> a.value) atom
    |> List.Extra.zip vars
    |> Dict.fromList

predicateStyleToggle : PredicateStyle -> Element Msg
predicateStyleToggle selected =
    Element.Input.radioRow  [ Element.padding 0
                            , Element.spacing 10
                            , Element.Font.size sizePalette.command
                            ]   { onChange = ChangePredicateStyle
                                , selected = Just selected
                                , label = Element.Input.labelLeft [Element.Font.size sizePalette.command] (Element.text "Output Format")
                                , options = [ Element.Input.option Verbose (Element.text "Verbose")
                                            , Element.Input.option Terse (Element.text "Terse")
                                            ]
                                }

graphToggle : GraphDisplay -> Element Msg
graphToggle selected =
    Element.Input.radioRow  [ Element.padding 0
                            , Element.spacing 10
                            , Element.Font.size sizePalette.command
                            ]   { onChange = ToggleGraph
                                , selected = Just selected
                                , label = Element.Input.labelLeft [Element.Font.size sizePalette.command] (Element.text "Graph Display")
                                , options = [ Element.Input.option On (Element.text "On")
                                            , Element.Input.option Off (Element.text "Off")
                                            ]
                                }

downloadFile : String -> String -> Cmd msg
downloadFile fileName query =
  Download.string fileName "text" query

--Element Based UI

colorPalette = 
    { header = Element.rgb255 77 195 230
    , background = Element.rgb255 255 247 250 
    , button = Element.rgb255 217 246 255
    , highlight = Element.rgb255 68 242 187
    , lowlight = Element.rgb255 34 86 102
    , material = Element.rgb255 255 255 255 
    }

sizePalette = 
    { smallPrint = materialPalette.overline.size  --10 -- overline
    , command = materialPalette.caption.size  -- 12  -- Caption
    , normal = materialPalette.body2.size  --14  -- body2, +bold caps for buttons.
    , highlight = materialPalette.h6.size --20 -- h6
    , subject = materialPalette.h4.size  --34 -- h4
    , input = materialPalette.body1.size  -- 16 -- body1
    }

type TypoWeight 
    = Light
    | Regular
    | Medium  

type TypoCase
    = Sentence
    | AllCaps

materialPalette =
    { h1 = { size = 96, weight = Light, case_ = Sentence }
    , h2 = { size = 60, weight = Light, case_ = Sentence }    
    , h3 = { size = 48, weight = Regular, case_ = Sentence }    
    , h4 = { size = 34, weight = Regular, case_ = Sentence }    
    , h5 = { size = 24, weight = Regular, case_ = Sentence }    
    , h6 = { size = 20, weight = Medium, case_ = Sentence }    
    , subtitle1 = { size = 16, weight = Regular, case_ = Sentence }    
    , subtitle2 = { size = 14, weight = Medium, case_ = Sentence }    
    , body1 = { size = 16, weight = Regular, case_ = Sentence }    
    , body2 = { size = 14, weight = Regular, case_ = Sentence }    
    , button = { size = 14, weight = Medium, case_ = AllCaps }    
    , caption = { size = 12, weight = Regular, case_ = Sentence }    
    , overline = { size = 10, weight = Regular, case_ = AllCaps }    
    }

elOfTabularResults : ServerVars -> ServerForm SelectAtom -> Element msgDecoder
elOfTabularResults vars result = 
    let
        data = List.map (\r -> makeDict vars r) result  -- List dict
        columns = List.map (\v ->   { header = Element.el   [ Element.Font.bold
                                                            , Element.Font.size sizePalette.input
                                                            ] (Element.text v)
                                    , width = Element.fill
                                    , view = (\x -> Dict.get v x |> Maybe.withDefault "" |> Element.text) 
                                    }) vars
    in
        Element.table   [ Element.Font.size sizePalette.normal
                        , Element.Border.width 2
                        ] {data = data, columns = columns}

elOfQueryHistory : List SparqlQuery -> Element Msg
elOfQueryHistory history =
    Element.textColumn [ spacing 10, padding 10, Element.height (Element.px 120), scrollbarY ]
        ( List.map (\query ->
-- Debug.log (Sparql.toString query)
                Element.row [spacing 10][ Element.Input.button
                                                [ Element.Background.color colorPalette.button 
                                                , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                                , Element.Border.rounded 5
                                                , Element.focused
                                                    [ Element.Background.color colorPalette.highlight ]
                                                ]
                                                { onPress = Just (FileLoaded (Sparql.toString query))
                                                , label = Element.text "^"
                                                }
                                        , Element.textColumn [ Element.Font.size sizePalette.normal
                                                    , Element.Border.dotted
                                                    , Element.Border.widthEach {bottom=1, top=0, left=0, right=0}
                                                    ] 
                                                    (List.map(\line ->
                                                        Element.paragraph [] [Element.text line]
                                                    ) (query |> Sparql.toString |> String.split "\n")) --    [ Element.text <| Sparql.toString query]
                                        ]
            ) history)


elOfUploadQueryFromFile : Element Msg
elOfUploadQueryFromFile =
    Element.row [ Element.spacing 5
                , Element.Font.size sizePalette.command
                , Element.padding 10
                , Element.height (Element.px 30)
                , Element.width Element.fill
                , Element.Background.color colorPalette.header
                ]   [ Element.Input.button  [ Element.Background.color colorPalette.button
                                            , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                            , Element.Border.rounded 5
                                            , Element.height (Element.px 20)
                                            , Element.width (Element.px 100)
                                            ]   { onPress = Just FileRequested
                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Load Query"
                                                }
                    , Element.Input.button  [ Element.Background.color colorPalette.button
                                            , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                            , Element.Border.rounded 5
                                            , Element.height (Element.px 20)
                                            , Element.width (Element.px 100)
                                            ]   { onPress = Just DownloadFile
                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Download Query"
                                                }
                    ]

elOfDownloadCsv : Element Msg
elOfDownloadCsv =
    Element.row [
                ]
                [ Element.Input.button [ Element.Background.color colorPalette.button
                                            , Element.mouseOver [ Element.Background.color colorPalette.highlight] 
                                            , Element.Border.rounded 5
                                            , Element.height (Element.px 20)
                                            , Element.width (Element.px 100)
                                            ]   { onPress = Just DownloadResultsAsCSV
                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Download"
                                                }]

elOfResultFormatToggle : Model -> Element Msg
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

elOfQueryPanel : Model -> Element Msg
elOfQueryPanel model =
    Element.column  [ Element.width Element.fill
                    , Element.spacingXY 0 5
                    , Element.Font.size sizePalette.input
                    ]   [ Element.Input.multiline []
                                { onChange = ChangeQuery
                                , text = (Sparql.toString model.query)
                                , placeholder = Nothing
                                , label = Element.Input.labelLeft [Element.centerY] (Element.none)
                                , spellcheck = False
                                }
                        , Element.row   [Element.paddingXY 8 0, Element.spacingXY 3 0]  
                                                [ Element.none 
                                                , Element.Input.button  [ Element.width (Element.px 60)
                                                                , Element.height (Element.px 30)
                                                                , Element.spacingXY 50 0 
                                                                , Element.Border.rounded 5
                                                                , Element.Background.color colorPalette.button
                                                                , Element.mouseOver [ Element.Background.color colorPalette.highlight]
                                                                ] { onPress = Just (SubmitQuery model.query)
                                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "Go"
                                                                }
                                                , Element.Input.button  [ Element.width (Element.px 150)
                                                                , Element.height (Element.px 30)
                                                                , Element.spacingXY 50 0 
                                                                , Element.Border.rounded 5
                                                                , Element.Background.color colorPalette.button
                                                                , Element.mouseOver [ Element.Background.color colorPalette.highlight]
                                                                ] { onPress = Just (ClearCaches)
                                                                , label = Element.el [ Element.Font.center, Element.width Element.fill ] <| Element.text "ClearCaches"
                                                                }
                                            ]
                        ]

elOfMainPage : Model -> Element Msg
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

elOfHeading : Model -> Element Msg
elOfHeading model =
    Element.el  [ Element.Background.color colorPalette.header
                , Element.height (Element.px 40)
                , Element.width Element.fill
                , Element.Font.size 12
                ] (Element.row  [ Element.centerY
                                , Element.width Element.fill
                                ]   
                                    [ elOfServerInput model
                                    , Element.el [Element.centerY, Element.alignRight, Element.paddingXY 5 0] (Element.text <| "Sparql Playground - "++version)
                                    ])

elOfServerInput : Model -> Element Msg
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
                                                                        , Element.Border.rounded 5
                                                                        , Element.Background.color colorPalette.button
                                                                        , Element.mouseOver [ Element.Background.color colorPalette.highlight]
                                                                        ]   
                                                                            { onPress = Just PingServer
                                                                            , label= (Element.text "Connect")
                                                                            }
                                                ])

view : Model -> Document Msg
view model = { title = "Sparql Query Playground - 0.0"
            , body = (List.singleton <|
                    case model.state of
                        Initialising ->
                                Element.layout []  (elOfHeading model)-- h1 [][Html.text "Sparql Query Playground v0.1"]
                        Pinging -> 
                            Element.layout [] (Element.text <| "Pinging"++model.server)
                        Querying -> 
                                Element.layout [] (elOfMainPage model)
                        Waiting -> 
                                Element.column  [ Element.width Element.fill] [ elOfMainPage model
                                                , Element.text  "Wating for server response..."
                                                ]
                                |> Element.layout []
                        ApiError error -> 
                            case error of
                                Http.BadBody err ->
                                    Element.layout [] (Element.column []
                                        [ Element.text "ApiError: I can't interpret the response"
                                        , Element.text err
                                        , Element.Input.button  [ Element.Background.color colorPalette.button
                                                                , Element.mouseOver [ Element.Background.color colorPalette.highlight]
                                                                , Element.Border.rounded 5
                                                                , Element.height (Element.px 20)
                                                                , Element.width (Element.px 100)
                                                                ]   
                                                                { onPress = Just BackToQuery
                                                                , label = Element.text "Back"
                                                                }
                                        ])
                                _ ->
                                    Element.layout [] ( Element.column []
                                        [ Element.text "ApiError: Oops - something went wrong! :-("
                                        , Element.Input.button  [ Element.Background.color colorPalette.button
                                                                , Element.mouseOver [ Element.Background.color colorPalette.highlight]
                                                                , Element.Border.rounded 5
                                                                , Element.height (Element.px 20)
                                                                , Element.width (Element.px 100)
                                                                ]   
                                                                { onPress = Just BackToQuery
                                                                , label = Element.text "Back"
                                                                }
                                        ])
                        DisplayingSelectError message ->
                                            Element.column  [ Element.width Element.fill]   
                                                                [ elOfMainPage model
                                                                , Element.paragraph [Element.Font.size sizePalette.normal] [(Element.text message)]
                                                            ]
                                            |> Element.layout []
                        DisplayingSelectResult vars result ->
                            case model.resultsDisplay of
                                Table ->
                                        Element.column  [ Element.width Element.fill] [ elOfMainPage model
                                                        , elOfDownloadCsv
                                                        , elOfLineOfThought model -- maybe not model???
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
                                            Element.column  [ Element.width Element.fill] [ elOfMainPage model
                                                            , elOfLineOfThought model -- maybe not model???
                                                            , predicateStyleToggle model.predicateStyle
                                                            , Element.column    [ Element.width (Element.px 1400)
--                                                                                , Element.height (Element.px 700)
                                                                                , Element.paddingXY 20 0
                                                                                ] --Element.fill
                                                            [ graphToggle model.graphDisplay
                                                            , case model.graphDisplay of
                                                                On ->
                                                                --    Debug.log (convertRdfDict2CommunityGraph a |>  Graph.DOT.output (\n -> RdfDict.rdfNodeToMaybeString (Tuple.first n) |> Maybe.withDefault "unknown" |> aka model.predicateStyle |> Just) 
                                                                --                                                                    (\e -> Just( aka model.predicateStyle e) ))
 --                                                                   convertRdfDict2CommunityGraph a |> GraphDisplay.init model.graphMaxIterations |> GraphDisplay.view |> Element.html 
--                                                                    rdfDictToJsonValue a |> sendActionRequestToWorker

                                                                    case model.graphImage of
                                                                        Available g -> 
--                                                                            Debug.log ("Centres"++(getX model.center |> String.fromFloat)++":"++(getY model.center |> String.fromFloat))
                                                                            svg  [ Attr.viewBox (getX model.center) (getY model.center) (g.graph.width/model.zoom) (g.graph.height/model.zoom)
                                            --                                                , Attr.width (TypedSvg.Types.px model.size.width)
                                            --                                                , Attr.height (TypedSvg.Types.px model.size.height)
                                                                                            , handleZoom Zoom
                                                                                            , Draggable.mouseTrigger () DragMsg
                                                                                            ] 
                                                                                            [generateDagreGraph model.size g model.center model.zoom]
                                                                                         |> Element.html
                                                                        Unavailable -> Element.text "No graph available"
                                                                        Requested -> Element.text "Graph not yet ready"
                                                                Off -> Element.none
                                                            ]
                                                            , elOfSubjects model
                                                            ]
                                            |> Element.layout []
                                        Nothing ->
                                            Element.column  [ Element.width Element.fill] [ elOfMainPage model
                                                        , elOfDownloadCsv
                                                        , elOfLineOfThought model -- maybe not model???
                                                        , elOfTabularResults vars result
                                            ]
                                            |> Element.layout []
  )}

handleZoom : (Float -> msg) -> Html.Attribute msg
handleZoom onZoom =
    let
        alwaysPreventDefaultAndStopPropagation msg =
            { message = msg, stopPropagation = True, preventDefault = True }

        zoomDecoder : Decoder msg
        zoomDecoder =
            JD.float
                |> JD.field "deltaY" 
                |> JD.map onZoom

    in
    Html.Events.custom
        "wheel"
    <| JD.map alwaysPreventDefaultAndStopPropagation zoomDecoder

-- Decoders
selectAtomDecoder : Decoder SelectAtom
selectAtomDecoder = 
    map5 SelectAtom
        (field "key" JD.string)
        (field "value" JD.string )
        (field "aType" JD.string )
        (field "language" JD.string )
        (field "datatype" JD.string )

mainDecoder : Decoder KGResponse
mainDecoder =
     map7 KGResponse
        (field "server" JD.string)
        (field "status" JD.int)
        (field "reason" JD.string)
        (field "queryType" JD.string)
        (field "query" JD.string)
        (field "vars" (JD.list JD.string))
        (field "result" (JD.list (JD.list selectAtomDecoder)))    
-- Main

main : Program (Maybe String ) Model Msg
main =
  Browser.application
    { init = initialFn
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlChange = handleUrlChange
    , onUrlRequest = handleUrlRequest
    }
