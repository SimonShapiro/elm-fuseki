module Domain exposing (..)
import Main exposing (SelectAtom)

type alias ToDo = 
    { id: String
    , description: String
    , status: String
    }

allToDos: (List String) -> (List(List SelectAtom)) -> (List ToDo)
allToDos preds list =
    [(ToDo "" "" "")]

makeToDo: (SelectAtom, List((SelectAtom, SelectAtom))) -> ToDo
makeToDo (id, predsObjects) = 
    let
        description = extractPredicate "http://example/com/descrption" predsObjects
                    |> List.head  -- only if cardinality of 1.
        status = extractPredicate "http://example.com/status" predsObjects
                |> List.head
    in
        ToDo id.value (Maybe.withDefault "" description) (Maybe.withDefault "" status)

viewToDos: List ToDo -> Html Msg      
viewToDos toDos =
    div []
    (List.map (\toDo -> 
        div []
            [ b [] [text toDo.id]
            , div []
                [ text "details:"
                , text toDo.description]
            , div []
                [ text "status:"
                , text toDo.status]
            , hr [][]
            ]
    ) toDos)
