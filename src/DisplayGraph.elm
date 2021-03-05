module DisplayGraph exposing (..)

import Color
import Html exposing (Html)
import TypedSvg exposing (circle, svg, rect)
import TypedSvg.Attributes as Attr exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox, x, y, width, height)
import TypedSvg.Types exposing (Paint(..), px)
import TypedSvg.Core exposing (Svg)
import Dagre exposing (..)

myCircle : Svg msg
myCircle =
    circle
        [ cx (px 100)
        , cy (px 100)
        , r (px 30)
        , fill <| Paint Color.blue
        , strokeWidth (px 2)
        , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
        ]
        []

frame : Float -> Float -> Svg msg
frame width height =
    rect 
        [ x (px 0)
        , y (px 0)
        , Attr.width (px width)
        , Attr.height (px height)
        , stroke <| Paint <| Color.black
        , strokeWidth (px 1)
        , fill <| Paint <| Color.white
        ]
        []


generateDagreGraph : Dagre.PlacedGraph -> Html msg
generateDagreGraph graph =   svg [ viewBox 0 0 graph.graph.width graph.graph.height ] 
    [ frame graph.graph.width graph.graph.height
    , myCircle 
    ]