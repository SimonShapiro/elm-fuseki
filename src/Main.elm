module Main exposing(..)

import Browser exposing (..)
import Html exposing(Html, div, text, input, button, h1, h2, h4, span, ul, li, b, p, hr, br, table, tr, th, td)
import Html.Attributes exposing (placeholder, value, class, rows, cols, wrap, style, type_, name, checked)
import Html.Events exposing (onInput, onClick)
import Browser.Events exposing (onKeyDown)
import Http
import Html exposing (textarea)
import Html.Events exposing (on)
import Json.Decode exposing (Decoder, Error, errorToString, field, string, map, map2, map3, map4, map5, map6, map7, list, int, decodeString, at, andThen)

import File exposing (File)
import File.Select as Select
import File.Download as Download
import Task
import List.Extra exposing (uncons, groupWhile)
import Maybe.Extra exposing (combine)
import Html exposing (a)

type Cardinality 
    = OneToOne 
    | ZeroOrOne 
    | ZeroToMany 
    | OneToMany 

type UIState 
    = Initialising 
    | Pinging 
    | Querying 
    | DisplayingSelectResult  (List String) (List (List SelectAtom))
    | DisplayingSelectError String
    | ApiError Http.Error
    | Waiting

type alias Model = 
    { state: UIState
    , server: Server
    , query: Sparql 
    , keyboard: KeyboardMode
    , resultsDisplay: ResultsDisplay
    , predicateStyle: PredicateStyle
    }

type alias SubjectOrientedAtoms = (List (SelectAtom, List(SelectAtom, SelectAtom)))  -- s, List (pred, obj)

type alias CollapsedPredicates = (List (SelectAtom, List (SelectAtom, List SelectAtom)))

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
    | SubmitQuery 
    | GotSparqlResponse (Result Http.Error KGResponse)
    | FileRequested 
    | FileSelected File
    | FileLoaded Sparql
    | DownloadFile
    | DownloadResultsAsCSV (List String) (List (List SelectAtom))
    | ChangeOutputFormat String
    | ChangePredicateStyle String
    | BackToQuery

type alias Server = String

type alias Sparql = String

type alias KGResponse =  -- a copy of the query is available in the api
    { server: Server
    , status: Int
    , message: String
    , queryType: String
    , query: Sparql
    , vars: (List String)
    , result: (List (List SelectAtom))
    }

type alias SelectAtom =
    { key: String
    , value: String
    }

type RdfNode 
    = Uri String
    | BlankNode String
    | LiteralOnlyValue {value: String}
    | LiteralValueAndDataType {value: String, dataType: String}
    | LiteralLanguageString {value: String, language: String}

type alias RdfKey = (String, String)  -- (simple, full)

type RdfDict = Dict RdfKey (List RdfNode)

type alias Nodes = List RdfDict

type alias Edges = List (RdfKey, RdfKey)

extractValues: List SelectAtom -> List String
extractValues rows =
    List.map (\r -> r.value) rows

extractPredicate: String -> List((SelectAtom, SelectAtom)) -> List String
extractPredicate spec preds =    -- will return [] if spec not present
    List.filter (\a -> (Tuple.first a).value == spec) preds
    |> List.unzip
    |> Tuple.second
    |> List.map (\obj -> obj.value)

selectAtomDecoder: Decoder SelectAtom
selectAtomDecoder = 
    map2 SelectAtom
        (field "key" string)
        (field "value" string )

server: Server
server = "http://localhost:port"

startWith: Model
startWith = Model Initialising server "" Normal Table Terse

initialModel: flags -> (Model, (Cmd Msg))
initialModel _ = (startWith, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)
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
                    Debug.log "Pinged OK"
                    ({model | state = Querying, keyboard = ReadyToAcceptControl}, Cmd.none)
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
        SubmitQuery -> 
            Debug.log ("Submitting Query "++model.query)
            ({model | state = Waiting}, submitQuery model.server model.query)
        GotSparqlResponse response -> 
            case response of
                Ok okData -> 
                    case okData.status of
                        200 ->
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
                (model, downloadFile "result.csv " <| String.join "\n" headedResults)
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

msgDecoder : Decoder Msg
msgDecoder =
    field "key" string
        |> map PressedKey

subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown msgDecoder

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

submitQuery: Server -> Sparql -> (Cmd  Msg)
submitQuery newServer query = 
        Http.request
                    { method = "POST"
                    , headers = [Http.header "Content-Type" "application/sparql-request"]
                    , url = newServer++"/sparql"
                    , body = Http.stringBody "text" query
                    , expect = Http.expectJson GotSparqlResponse mainDecoder
                    , timeout = Nothing
                    , tracker = Nothing
                    }

submitParametrisedQuery: Server -> Sparql -> (Http.Expect msg) -> (Cmd msg)
submitParametrisedQuery newServer query returnTo = 
        Http.request
                    { method = "POST"
                    , headers = [Http.header "Content-Type" "application/sparql-request"]
                    , url = newServer++"/sparql"
                    , body = Http.stringBody "text" query
                    , expect = returnTo
                    , timeout = Nothing
                    , tracker = Nothing
                    }

makeTriple: List SelectAtom -> Maybe (SelectAtom, SelectAtom, SelectAtom)
makeTriple spo =
    case (List.length spo) of
       3 -> Maybe.map3 (\a b c -> (a, b, c)) (List.head spo) (List.head (List.drop 1 spo)) (List.head (List.drop 2 spo))
       _ -> Nothing

extractTriples:  List (List SelectAtom) -> Maybe (List (SelectAtom, SelectAtom, SelectAtom))
extractTriples results = 
    List.map (\result -> makeTriple result) results
    |> combine

pivotToSubject: Maybe (List (SelectAtom, SelectAtom, SelectAtom)) -> Maybe SubjectOrientedAtoms
pivotToSubject maybeList =
            case maybeList of
                Nothing -> Nothing
                Just l -> l
                    |> List.map (\triple -> rearrangeTriple triple)
                    |> groupWhile (\a b -> (Tuple.first a).value == (Tuple.first b).value)
                    |> separateIntoSubject_PredicateObjects
                    |> Just

rearrangeTriple: (a, b, c) -> (a, (b, c))
rearrangeTriple (a, b, c) = 
    (a, (b, c))

separateIntoSubject_PredicateObjects: List (( SelectAtom, (SelectAtom, SelectAtom) )
                                            , List ( SelectAtom, (SelectAtom, SelectAtom) )) 
                                            -> List (SelectAtom, List(SelectAtom, SelectAtom))
separateIntoSubject_PredicateObjects l = 
    l |> List.map (\subject ->
                        let
                            subj = Tuple.first (Tuple.first subject)
                            predobj = Tuple.second (Tuple.first subject)
                            predicateObjects = predobj :: Tuple.second (List.unzip (Tuple.second subject))

                        in
                            (subj, predicateObjects)
                    )

aka: PredicateStyle -> String -> String
aka predicateStyle pred = 
    case predicateStyle of
       Verbose -> pred
       Terse -> String.split "/" pred
                |> List.reverse
                |> List.head
                |> Maybe.withDefault pred

-- pivotToSubject: (List (List SelectAtom)) -> List (List (SelectAtom, (List SelectAtom)))   --   (subject, [pred, obj])
-- pivotToSubject results =
--     let
--         subjects = List.filterMap (\triple -> uncons triple) results -- List (SelectAtom, List SelectAtom) ie ?s of [?s ?p ?o])
--             |> groupWhile (\a b -> (Tuple.first a).value == (Tuple.first b).value)
-- --            |> List.map (\group -> (Tuple.first group)::(Tuple.second group))
-- --            |> List.map (\subject -> List.map (\group -> makeTriple group) subject) 
--     in
--         -- Debug.log (String.join ";" (List.map (\s -> s.value) subjects))
--         subjects
    
viewSubjects: PredicateStyle -> Maybe SubjectOrientedAtoms -> Html Msg      
viewSubjects predicateStyle subjectPredObjs =
    case subjectPredObjs of 
        Nothing -> div[][]
        Just subjs ->
            div []
            (List.map (\spo -> 
                let
                    subject = Tuple.first spo
                    predObjs = Tuple.second spo
                in
                    div []
                        [ b [] [text subject.value]
                        , div [] (List.map (\details ->
                                    div[]
                                        [ b []  [ text (aka predicateStyle (Tuple.first details).value)
                                                , text ": "]
                                        , text (Tuple.second details).value
                                        ]
                            ) predObjs)
                        , hr [][]
                        ]
            ) subjs)
--            |> List.concat


-- View


-- could refactor below into two steps - shape and show
tableView: (List String) -> (List (List SelectAtom)) -> Html Msg
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
                , button [onClick SubmitQuery][text "Submit"]
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

view: Model -> Html Msg
view model = 
    case model.state of
        Initialising ->
            div [] 
                [ h1 [][text "hello world"]
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
                    div []                
                        [ uploadQueryFromFile
                        , queryInput model.server model.query
                        , resultFormatToggle model.resultsDisplay
                        , button [onClick <| DownloadResultsAsCSV vars result][text "Download csv"]
                        , tableView vars result
                        ]
                SubjectOrientation ->
                    case vars of
                        ["s", "p", "o"] ->
                            div []                
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , resultFormatToggle model.resultsDisplay
                                , h2 [][text "Subject orientation"]
                                , predicateStyleToggle model.predicateStyle
                                , br [] []
                                , viewSubjects model.predicateStyle (pivotToSubject <| extractTriples result)
                                ]
                        _ ->                             
                            div []                
                                [ uploadQueryFromFile
                                , queryInput model.server model.query
                                , resultFormatToggle model.resultsDisplay
                                , h4 [][text "Subject orientation only where results are in the shape of ?s ?p ?o"]
                                ]

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
  Browser.element
    { init = initialModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }