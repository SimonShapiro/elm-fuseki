module Dagre exposing (..)
import TypedSvg.Types exposing (Length(..))

type RankDir
    = TB
    | BT
    | LR
    | RL

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
defaultG =  { rankDir = TB
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
    }

type alias Point = 
    { x : Float
    , y: Float
    }

type alias PlacedEdge = 
    { from : Int
    , to : Int
    , label : String
    -- , x : Int
    -- , y : Int
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
    , edges : List UnplacedEdge
    }