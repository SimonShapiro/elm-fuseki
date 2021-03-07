module DisplayGraph exposing (..)

import Color
import Html exposing (Html)
import TypedSvg exposing (circle, svg, rect, g, text_)
import TypedSvg.Attributes as Attr exposing (x, y, cx, cy, fill, r, stroke, strokeWidth, viewBox, x, y, width, height, title)
import TypedSvg.Types exposing (Paint(..), px)
import TypedSvg.Core exposing (Svg, text)
import Dagre exposing (..)

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
            , circle
                [ cx (px x)
                , cy (px y)
                , r (px 2)
                , fill <| Paint Color.blue
                , strokeWidth (px 2)
                , stroke <| Paint <| Color.rgba 0.8 0 0 0.5
                , title label
                ]
                []
            , text_ [ Attr.x (px x)
                    , Attr.y (px y)
                    , fill <| Paint <| Color.black
                    , Attr.fontSize (px 12)
                    , Attr.textAnchor TypedSvg.Types.AnchorMiddle 
--                    , strokeWidth (px 1)
                    ] 
                        [ case aka label of
                            Nothing -> text ""
                            Just a -> text a
                        ]
    ]

aka : String -> Maybe String
aka long = 
    long
    |> String.split "/"
    |> List.reverse
    |> List.head

shapeEdgePoints : List Point -> String
shapeEdgePoints points = 
    let
        head = points |> List.head |> Maybe.withDefault {x=0, y=0}
        rest = points |> List.tail |> Maybe.withDefault [{x=0, y=0}]
    in
        "M"::((String.fromFloat head.x)++","++(String.fromFloat head.y))
        ::"Q":: List.map (\p -> (String.fromFloat p.x)++","++(String.fromFloat p.y)) rest
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
                        ] []
                , text_ [ Attr.x (px edge.x)
                    , Attr.y (px edge.y)
                    , fill <| Paint <| Color.black
                    , Attr.fontSize (px 8)
                    , Attr.textAnchor TypedSvg.Types.AnchorEnd 
--                    , strokeWidth (px 1)
                    ] 
                        [ case aka edge.label of
                            Nothing -> text ""
                            Just a -> text a
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

generateDagreGraph : Dagre.PlacedGraph -> Html msg
generateDagreGraph graph =   svg [ viewBox 0 0 graph.graph.width graph.graph.height ] 
    [ frame graph.graph.width graph.graph.height
    , g [Attr.transform [TypedSvg.Types.Scale 0.9 0.9, TypedSvg.Types.Translate 10 10]] [ displayEdges graph.edges
                                                                                        , displayNodes graph.nodes 
                                                                                        ]
    ]