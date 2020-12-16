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