module W.Chart exposing
    ( Config
    , config
    , globalStyles
    , view
    , withYData
    , withYDataset
    , withZData
    , withZDataset
    )

import Axis
import Html as H
import Html.Attributes as HA
import Scale
import Svg
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal exposing (RenderData(..))


type alias Config msg x y z constraints =
    W.Chart.Internal.Config msg x y z constraints



-- Constants


lineStrokeWidth : Float
lineStrokeWidth =
    2


xAxisPadding : Float
xAxisPadding =
    22


yAxisPadding : Float
yAxisPadding =
    62


labelFontSize : Float
labelFontSize =
    13



-- Datasets


config :
    List option
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> W.Chart.Internal.Config msg x y z {}
config opts axisData =
    W.Chart.Internal.Config
        { attrs = []
        , yData = Nothing
        , zData = Nothing
        , xData =
            { data = axisData.data
            , toLabel = axisData.toLabel
            , toColor = \_ -> ""
            , toValue = \_ _ -> Nothing
            }
        }


withYData :
    List option
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x any z constraints
    -> Config msg x () z { with | yData : () }
withYData opts axisData (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config
        { attrs = cfg.attrs
        , xData = cfg.xData
        , zData = cfg.zData
        , yData =
            Just
                { data = [ () ]
                , toLabel = \_ -> axisData.label
                , toColor = \_ -> axisData.color
                , toValue = \_ -> axisData.toValue
                }
        }


withYDataset :
    List option
    ->
        { data : List y
        , toLabel : y -> String
        , toColor : y -> String
        , toValue : y -> x -> Maybe Float
        }
    -> Config msg x any z constraints
    -> Config msg x y z { with | yData : () }
withYDataset opts axisData (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config
        { attrs = cfg.attrs
        , xData = cfg.xData
        , zData = cfg.zData
        , yData = Just axisData
        }


withZData :
    List option
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x y any constraints
    -> Config msg x y () { with | zData : () }
withZData opts axisData (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config
        { attrs = cfg.attrs
        , xData = cfg.xData
        , yData = cfg.yData
        , zData =
            Just
                { data = [ () ]
                , toLabel = \_ -> axisData.label
                , toColor = \_ -> axisData.color
                , toValue = \_ -> axisData.toValue
                }
        }


withZDataset :
    List option
    ->
        { data : List z
        , toLabel : z -> String
        , toColor : z -> String
        , toValue : z -> x -> Maybe Float
        }
    -> Config msg x y any constraints
    -> Config msg x y z { with | zData : () }
withZDataset opts axisData (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config
        { attrs = cfg.attrs
        , xData = cfg.xData
        , yData = cfg.yData
        , zData = Just axisData
        }



-- VerticalScales
-- Bounds
--  View


view : List (W.Chart.Internal.Element msg x y z constraints) -> Config msg x y z constraints -> H.Html msg
view elements cfg =
    let
        renderData : RenderData msg x y z constraints
        renderData =
            W.Chart.Internal.toRenderData cfg
    in
    H.div
        [ HA.class "ew-charts" ]
        [ Svg.svg
            [ viewBox renderData ]
            [ -- Grid
              viewYGrid renderData
            , viewXGrid renderData

            -- Labels
            , viewLabels renderData

            -- Axis
            , viewXAxis renderData
            , viewYAxis renderData
            , viewZAxis renderData

            -- Elements
            , viewElements .main renderData elements
            , viewElements .foreground renderData elements
            ]
        ]


viewBox : RenderData msg x y z constraints -> Svg.Attribute msg
viewBox (RenderData d) =
    SA.viewBox 0 0 d.spacings.canvas.width d.spacings.canvas.height



-- Elements


viewElements :
    (W.Chart.Internal.ElementData msg x y z constraints -> Maybe (RenderData msg x y z constraints -> Svg.Svg msg))
    -> RenderData msg x y z constraints
    -> List (W.Chart.Internal.Element msg x y z constraints)
    -> SC.Svg msg
viewElements fn renderData elements =
    elements
        |> List.filterMap (\(W.Chart.Internal.Element el) -> Maybe.map (\el_ -> el_ renderData) (fn el))
        |> S.g []



-- Labels


viewLabels : RenderData msg x y z constraints -> SC.Svg msg
viewLabels (RenderData d) =
    S.g
        []
        [ d.attrs.yAxis.label
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { x = d.attrs.padding * 0.5 + labelFontSize * 0.5
                        , y = d.spacings.padding.top + d.spacings.chart.height * 0.5
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 270 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
                            , SAP.fontSize labelFontSize
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        , d.attrs.zAxis.label
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { x = d.spacings.canvas.width - d.attrs.padding * 0.5 - labelFontSize * 0.5
                        , y = d.spacings.padding.top + d.spacings.chart.height * 0.5
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 90 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
                            , SAP.fontSize labelFontSize
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        , d.attrs.xAxis.label
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { x = d.spacings.padding.left + d.spacings.chart.width * 0.5
                        , y = d.spacings.canvas.height - d.attrs.padding * 0.5 + labelFontSize * 0.5
                        }
                        [ S.text_
                            [ SA.textAnchor ST.AnchorMiddle
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
                            , SAP.fontSize labelFontSize
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        ]



--  Axes & Lines


viewYGrid : RenderData msg x y z constraints -> SC.Svg msg
viewYGrid (RenderData d) =
    case ( d.attrs.yAxis.showGrid, d.y ) of
        ( True, Just yData ) ->
            Scale.ticks yData.scale d.attrs.yAxis.ticks
                |> List.map
                    (\tick ->
                        let
                            y : Float
                            y =
                                Scale.convert yData.scale tick
                        in
                        S.line
                            [ SA.x1 (ST.px 0)
                            , SA.x2 (ST.px d.spacings.chart.width)
                            , SA.y1 (ST.px y)
                            , SA.y2 (ST.px y)
                            , SA.strokeWidth (ST.px 1.0)
                            , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                            ]
                            []
                    )
                |> W.Chart.Internal.viewTranslateChart d.spacings

        _ ->
            H.text ""


viewXGrid : RenderData msg x y z constraints -> SC.Svg msg
viewXGrid (RenderData d) =
    case ( d.attrs.yAxis.showGrid, d.y ) of
        ( True, Just yData ) ->
            Scale.ticks d.x.scale d.attrs.xAxis.ticks
                |> List.map
                    (\tick ->
                        let
                            x : Float
                            x =
                                Scale.convert d.x.scale tick
                        in
                        S.line
                            [ SA.y1 (ST.px 0)
                            , SA.y2 (ST.px d.spacings.chart.height)
                            , SA.x1 (ST.px x)
                            , SA.x2 (ST.px x)
                            , SA.strokeWidth (ST.px 1.0)
                            , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                            ]
                            []
                    )
                |> W.Chart.Internal.viewTranslateChart d.spacings

        _ ->
            H.text ""


viewXAxis : RenderData msg x y z constraints -> SC.Svg msg
viewXAxis (RenderData d) =
    if d.attrs.xAxis.showAxis then
        W.Chart.Internal.viewTranslate
            { x = d.spacings.padding.left
            , y = d.spacings.padding.top + d.spacings.chart.height
            }
            [ S.g
                [ SA.class [ "un-charts--x-axis" ] ]
                [ Axis.bottom
                    [ Axis.tickCount 5
                    , Axis.tickSizeOuter 0
                    , Axis.tickSizeInner 6
                    , Axis.tickPadding 6
                    , Axis.tickFormat d.x.toLabel
                    ]
                    d.x.scale
                ]
            ]

    else
        H.text ""


viewYAxis : RenderData msg x y z constraints -> SC.Svg msg
viewYAxis (RenderData d) =
    case ( d.attrs.yAxis.showAxis, d.y ) of
        ( True, Just yData ) ->
            W.Chart.Internal.viewTranslate
                { x = d.spacings.padding.left
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "un-charts--y-axis" ] ]
                    [ viewAxis
                        Axis.left
                        { ticks = 5
                        , format = d.attrs.yAxis.format
                        , scale = yData.scale
                        }
                    ]
                ]

        _ ->
            H.text ""


viewZAxis : RenderData msg x y z constraints -> SC.Svg msg
viewZAxis (RenderData d) =
    case ( d.attrs.zAxis.showAxis, d.z ) of
        ( True, Just zData ) ->
            W.Chart.Internal.viewTranslate
                { x = d.spacings.padding.left + d.spacings.chart.width
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "un-charts--z-axis" ] ]
                    [ viewAxis
                        Axis.right
                        { ticks = 5
                        , format = d.attrs.zAxis.format
                        , scale = zData.scale
                        }
                    ]
                ]

        _ ->
            H.text ""



-- View Helpers


viewAxis :
    (List (Axis.Attribute a) -> Axis.RenderableScale x domain range a -> SC.Svg msg)
    ->
        { ticks : Int
        , format : a -> String
        , scale : Axis.RenderableScale x domain range a
        }
    -> SC.Svg msg
viewAxis axis props =
    axis
        [ Axis.tickCount props.ticks
        , Axis.tickSizeOuter 0
        , Axis.tickSizeInner 6
        , Axis.tickPadding 6
        , Axis.tickFormat props.format
        ]
        props.scale


viewHLine :
    { yValue : Float
    , strokeWidth : Float
    , stroke : String
    , xScale : Axis.RenderableScale x domain range Float
    , yScale : Axis.RenderableScale x domain range Float
    }
    -> SC.Svg msg
viewHLine props =
    S.line
        [ SA.x1 (ST.px (Tuple.first (Scale.rangeExtent props.xScale)))
        , SA.x2 (ST.px (Tuple.second (Scale.rangeExtent props.xScale)))
        , SA.y1 (ST.px (Scale.convert props.yScale props.yValue))
        , SA.y2 (ST.px (Scale.convert props.yScale props.yValue))
        , SA.strokeWidth (ST.px props.strokeWidth)
        , Svg.Attributes.stroke props.stroke
        ]
        []


viewVLine :
    { xValue : Float
    , strokeWidth : Float
    , stroke : String
    , xScale : Axis.RenderableScale x domain range Float
    , yScale : Axis.RenderableScale x domain range Float
    }
    -> SC.Svg msg
viewVLine props =
    S.line
        [ SA.x1 (ST.px (Scale.convert props.xScale props.xValue))
        , SA.x2 (ST.px (Scale.convert props.xScale props.xValue))
        , SA.y1 (ST.px (Tuple.first (Scale.rangeExtent props.yScale)))
        , SA.y2 (ST.px (Tuple.second (Scale.rangeExtent props.yScale)))
        , SA.strokeWidth (ST.px props.strokeWidth)
        , Svg.Attributes.stroke props.stroke
        ]
        []



-- Spacings
-- Styles


globalStyles : SC.Svg msg
globalStyles =
    H.node "style"
        []
        [ H.text ("""
            .ew-charts .tick text {
                fill: """ ++ Theme.baseAux ++ """;
                font-family: var(--theme-font-text), sans-serif;
                font-size: 12px;
            }

            .ew-charts--x-axis path.domain {
                stroke: """ ++ Theme.baseAux ++ """;
                stroke-width: """ ++ String.fromFloat lineStrokeWidth ++ """px;
            }
            .ew-charts--x-axis .tick line {
                stroke: """ ++ Theme.baseAux ++ """;
            }

            .ew-charts--y-axis path.domain,
            .ew-charts--y-axis .tick line {
                display: none;
            }

            .ew-charts--hover-line,
            .ew-charts--hover-circle,
            .ew-charts--tooltip {
                pointer-events: none;
            }

            .ew-charts--hover-circle {
                filter: drop-shadow(0px 0px 8px currentColor);
            }

            .ew-charts--hover-rect,
            .ew-charts--hover-line,
            .ew-charts--hover-circle,
            .ew-charts--tooltip {
                opacity: 0;
            }

            .ew-charts--hover-rect {
                fill: """ ++ Theme.baseAuxWithAlpha 0.05 ++ """;
            }
            .ew-charts--hover-rect.m--use-bars:hover,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-line,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-circle,
            .ew-charts--hover-rect:hover + g + .ew-charts--tooltip {
                opacity: 1;
            }

            .ew-charts--hover-content {
                pointer-events: none;
                opacity: 0;
                transition: 400;
            }
            .ew-charts--hover-target:hover + .ew-charts--hover-content {
                opacity: 1;
            }
            """)
        ]
