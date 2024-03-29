module DisplayGraph exposing (..)

import Color
import Html exposing (Html)
import TypedSvg exposing (circle, svg, rect, g, text_, polygon, marker, defs)
import TypedSvg.Attributes as Attr exposing (x, y, cx, cy, fill, r, stroke, strokeWidth, viewBox, x, y, width, height, title, points, orient)
import TypedSvg.Types exposing (Paint(..), px)
import TypedSvg.Core exposing (Svg, text)
import Dagre exposing (..)
import Draggable
import Math.Vector2 as Vector2 exposing (Vec2, getX, getY)
import Array exposing (length)

type Msg = 
    DragMsg (Draggable.Msg ())
    | OnDragBy Vec2
    | Zoom Float

type alias Size num =
    { width : num
    , height : num
    }


myIcon : Float -> Float -> String -> Int -> Int -> Svg msg
myIcon x y label wd ht =
    g []    [ rect  [ x - (toFloat wd/2) |> px |> Attr.x
                    , y - (toFloat ht/2) |> px |> Attr.y 
                    , height (px (toFloat ht))
                    , width (px (toFloat wd))
                    , strokeWidth (px 1)
                    , stroke <| Paint <| Color.black
                    , fill <| Paint <| Color.white
                    ]
                    []
            -- , circle
            --     [ cx (px x)
            --     , cy (px y)
            --     , r (px 2)
            --     , fill <| Paint Color.blue
            --     , strokeWidth (px 2)
            --     , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
            --     , title label
            --     ]
            --     []
            , text_ [ Attr.x (px x)
                    , Attr.y (px y)
                    , fill <| Paint <| Color.black
                    , Attr.fontSize (px 10)
                    , Attr.textAnchor TypedSvg.Types.AnchorMiddle 
--                    , strokeWidth (px 1)
                    ] 
                        [ aka 40 label |> text
                        ]
    ]

aka : Int -> String -> String
aka maxLength long =
    let
        short = long
            |> String.split "/"
            |> List.reverse
            |> List.head
            |> Maybe.withDefault long
    in 
        if short == ""
        then
            String.left (min maxLength (String.length long)) long
        else
            String.left (min maxLength (String.length short)) short


arrowHead = marker [ Attr.markerWidth (px 10)
                    , Attr.markerHeight (px 10)
                    , Attr.orient "auto"
                    , Attr.id "arrowHead"
                    , Attr.refX "10"
                    , Attr.refY "3.5"
                    ]
                    [ polygon [ points [(0,0), (10, 3.5), (0,7)]] [] ]

shapeEdgePoints : List Point -> String
shapeEdgePoints points = 
    let
        head = points |> List.head |> Maybe.withDefault {x=0, y=0}
        rest = points |> List.tail |> Maybe.withDefault [{x=0, y=0}]
    in
        "M"::((String.fromFloat head.x)++","++(String.fromFloat head.y))
        ::"L":: List.map (\p -> (String.fromFloat p.x)++","++(String.fromFloat p.y)) rest
        |> String.join " "

myEdge : Dagre.PlacedEdge -> Svg msg
myEdge edge =
    let
        points = edge.points
        -- start = points 
        --         |> List.head
        --         |> Maybe.withDefault {x=0, y=0}
        -- end = points
        --         |> List.reverse
        --         |> List.head
        --         |> Maybe.withDefault {x=0, y=0}
    in
        g [] [ TypedSvg.path   [ Attr.d (shapeEdgePoints points)
                        , Attr.stroke (Paint Color.black)
                        , Attr.fill PaintNone
                        , Attr.markerEnd "url(#arrowHead)"
                        ] []
                , text_ [ Attr.x (px edge.x)
                    , Attr.y (px edge.y)
                    , fill <| Paint <| Color.black
                    , Attr.fontSize (px 8)
                    , Attr.textAnchor TypedSvg.Types.AnchorEnd 
--                    , strokeWidth (px 1)
                    ] 
                        [ aka 100 edge.label |> text
                        ]
        ]

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

displayNodes : List Dagre.PlacedNode -> Svg msg
displayNodes nodes =
    g [] (List.map (\n -> myIcon n.x n.y n.label n.width n.height)
        nodes)

displayEdges : List Dagre.PlacedEdge -> Svg msg
displayEdges edges = 
    g [] (List.map (\e -> myEdge e) edges)

generateDagreGraph : Size Float -> Dagre.PlacedGraph -> Vector2.Vec2 -> Float -> Svg msg
generateDagreGraph size graph center zoom =  
    let
        ( cx, cy ) =
            ( getX center, getY center )

        ( halfWidth, halfHeight ) =
            ( size.width / 2, size.height / 2 )

        ( top, left ) =
            ( cy - halfHeight, cx - halfWidth )

        ( bottom, right ) =
            ( cy + halfHeight, cx + halfWidth )
    in
--        Debug.log ("Center="++(String.fromFloat cx)++" half "++(String.fromFloat halfWidth))
--        Debug.log ("Zoom="++(String.fromFloat zoom))
        g []
        [ defs [][arrowHead]
        , frame size.width size.height
        , g [-- Attr.viewBox 1000 1000 graph.graph.width graph.graph.height
             Attr.transform [TypedSvg.Types.Scale zoom zoom
                            , TypedSvg.Types.Translate ((getX center)*(-1)/zoom) ((getY center)*(-1)/zoom)]--left right top bottom] 
            ]
            [ displayEdges graph.edges
            , displayNodes graph.nodes 
            ]
        ]