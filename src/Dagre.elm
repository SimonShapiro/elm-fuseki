module Dagre exposing (..)

type RankDir
    = TB
    | BT
    | LR
    | RL

rankDirToString : RankDir -> String
rankDirToString rank = 
    case rank of 
        TB -> "TB"
        BT -> "BT"
        LR -> "LR"
        RL -> "RL"

type Align
    = UL
    | UR
    | DL
    | DR

type alias GraphOptions = 
    { rankDir : RankDir
    , align : Maybe Align
    }

defaultG : GraphOptions 
defaultG =  { rankDir = LR
            , align = Nothing
            }

type alias UnplacedNode = 
    { id : Int
    , label : String
    , width : Int
    , height : Int
    }

type alias PlacedNode = 
    { id : Int
    , label : String
    , width : Int
    , height : Int
    , x : Float
    , y : Float
    }

type alias UnplacedEdge =
    { from : Int
    , to : Int
    , label : String
    , width : Int
    , height : Int
    }

type alias Point = 
    { x : Float
    , y: Float
    }

type alias PlacedEdge = 
    { from : Int
    , to : Int
    , label : String
    , width : Int
    , height : Int
    , x : Float
    , y : Float
    , points : List Point
    }

type alias UnplacedDagreGraph =
    { options : GraphOptions
    , nodes : List UnplacedNode
    , edges : List UnplacedEdge
    }

type alias PlacedGraph = 
    { graph : {width : Float, height : Float}
    , nodes : List PlacedNode
    , edges : List PlacedEdge
    }