module W.Chart exposing
    ( config, width, ratio, padding, background, noTooltip, tooltipByNearest, binPaddingInner, binPaddingOuter, htmlAttrs, ChartAttribute, Config
    , view, ChartElement
    , withYData, withYDataset, withZData, withZDataset
    , axisLabel, defaultValue, format, logarithmic, noAxisLine, noGridLines, safety, stacked, stackedRelative, ticks, AxisAttribute
    , globalStyles
    , debug
    )

{-|

@docs config, width, ratio, padding, background, noTooltip, tooltipByNearest, binPaddingInner, binPaddingOuter, htmlAttrs, ChartAttribute, Config
@docs view, ChartElement
@docs withYData, withYDataset, withZData, withZDataset
@docs axisLabel, defaultValue, format, logarithmic, noAxisLine, noGridLines, safety, stacked, stackedRelative, ticks, AxisAttribute
@docs globalStyles

-}

import Attr
import Axis
import Dict
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
import W.Chart.Internal.Voronoi
import W.Chart.Tooltip


{-| -}
type alias Config msg x y z datasets =
    W.Chart.Internal.Config msg x y z datasets


{-| -}
type alias ChartElement msg x y z datasets =
    W.Chart.Internal.ChartElement msg x y z datasets


{-| -}
type alias ChartAttribute msg =
    Attr.Attr (W.Chart.Internal.Attributes msg)


{-| -}
type alias AxisAttribute =
    Attr.Attr W.Chart.Internal.AxisAttributes



-- Constants


lineStrokeWidth : Float
lineStrokeWidth =
    2


labelFontSize : Float
labelFontSize =
    13



-- Config


config :
    List (ChartAttribute msg)
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> W.Chart.Internal.Config msg x y z {}
config =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs axisData ->
            W.Chart.Internal.Config
                { attrs = attrs
                , yData = Nothing
                , zData = Nothing
                , xData =
                    { data = axisData.data
                    , toLabel = axisData.toLabel
                    , toColor = \_ -> ""
                    , toValue = \_ _ -> Nothing
                    }
                }
        )



-- Config : Attributes


width : Int -> ChartAttribute msg
width v =
    Attr.attr (\a -> { a | width = toFloat v })


ratio : Float -> ChartAttribute msg
ratio v =
    Attr.attr (\a -> { a | ratio = v })


padding : Int -> ChartAttribute msg
padding v =
    Attr.attr (\a -> { a | padding = toFloat v })


binPaddingOuter : Int -> ChartAttribute msg
binPaddingOuter v =
    Attr.attr (\a -> { a | binPaddingOuter = toFloat v })


binPaddingInner : Int -> ChartAttribute msg
binPaddingInner v =
    Attr.attr (\a -> { a | binPaddingInner = toFloat v })


background : String -> ChartAttribute msg
background v =
    Attr.attr (\a -> { a | background = v })


noTooltip : ChartAttribute msg
noTooltip =
    Attr.attr (\a -> { a | hoverTarget = Nothing })


tooltipByNearest : ChartAttribute msg
tooltipByNearest =
    Attr.attr (\a -> { a | hoverTarget = Just W.Chart.Internal.NearestPoint })


htmlAttrs : List (H.Attribute msg) -> ChartAttribute msg
htmlAttrs v =
    Attr.attr (\a -> { a | htmlAttributes = v })


debug : ChartAttribute msg
debug =
    Attr.attr (\a -> { a | debug = True })



-- Datasets


withYData :
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x any z datasets
    -> Config msg x () z { datasets | yData : () }
withYData =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | yAxis = axisAttrs }
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
        )


withYDataset :
    List AxisAttribute
    ->
        { data : List y
        , toLabel : y -> String
        , toColor : y -> String
        , toValue : y -> x -> Maybe Float
        }
    -> Config msg x any z datasets
    -> Config msg x y z { datasets | yData : () }
withYDataset =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | yAxis = axisAttrs }
                , xData = cfg.xData
                , zData = cfg.zData
                , yData = Just axisData
                }
        )


withZData :
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x y any datasets
    -> Config msg x y () { datasets | zData : () }
withZData =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | zAxis = axisAttrs }
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
        )


withZDataset :
    List AxisAttribute
    ->
        { data : List z
        , toLabel : z -> String
        , toColor : z -> String
        , toValue : z -> x -> Maybe Float
        }
    -> Config msg x y any datasets
    -> Config msg x y z { datasets | zData : () }
withZDataset =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | zAxis = axisAttrs }
                , xData = cfg.xData
                , yData = cfg.yData
                , zData = Just axisData
                }
        )



-- Axis Attributes


{-| -}
axisLabel : String -> AxisAttribute
axisLabel v =
    Attr.attr (\attrs -> { attrs | label = Just v })


{-| -}
defaultValue : Float -> AxisAttribute
defaultValue v =
    Attr.attr (\attrs -> { attrs | defaultValue = v })


{-| -}
format : (Float -> String) -> AxisAttribute
format v =
    Attr.attr (\attrs -> { attrs | format = v })


{-| -}
safety : Float -> AxisAttribute
safety v =
    Attr.attr (\attrs -> { attrs | safety = v })


{-| -}
ticks : Int -> AxisAttribute
ticks v =
    Attr.attr (\attrs -> { attrs | ticks = v })


{-| -}
stacked : AxisAttribute
stacked =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.AbsoluteStack })


{-| -}
stackedRelative : AxisAttribute
stackedRelative =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.RelativeStack })


{-| -}
logarithmic : Float -> AxisAttribute
logarithmic basis =
    Attr.attr (\attrs -> { attrs | scale = W.Chart.Internal.Logarithmic basis })


{-| -}
noAxisLine : AxisAttribute
noAxisLine =
    Attr.attr (\attrs -> { attrs | showAxis = False })


{-| -}
noGridLines : AxisAttribute
noGridLines =
    Attr.attr (\attrs -> { attrs | showGridLines = False })



-- View


view : List (W.Chart.Internal.ChartElement msg x y z datasets) -> Config msg x y z datasets -> H.Html msg
view widgets cfg =
    let
        renderData : RenderData msg x y z datasets
        renderData =
            W.Chart.Internal.toRenderData cfg

        (W.Chart.Internal.RenderData d) =
            renderData
    in
    H.div
        [ HA.class "ew-charts"
        , HA.classList
            [ ( "m--unfocus", d.attrs.hoverFocus && d.attrs.hoverTarget /= Nothing )
            , ( "m--debug", d.attrs.debug )
            ]
        ]
        [ Svg.svg
            [ SA.viewBox 0 0 d.spacings.canvas.width d.spacings.canvas.height
            , SA.class [ "ew-charts--svg" ]
            ]
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
            , viewChartElements "bg" .background renderData widgets
            , viewChartElements "main" .main renderData widgets
            , viewChartElements "fg" .foreground renderData widgets

            -- Hover
            , viewHover renderData widgets
            ]
        ]



-- Static Elements


viewChartElements :
    String
    -> (W.Chart.Internal.ChartElementData msg x y z datasets -> Maybe (RenderData msg x y z datasets -> Svg.Svg msg))
    -> RenderData msg x y z datasets
    -> List (W.Chart.Internal.ChartElement msg x y z datasets)
    -> SC.Svg msg
viewChartElements class getter renderData widgets =
    widgets
        |> List.filterMap (\(W.Chart.Internal.ChartElement w) -> Maybe.map (\el_ -> el_ renderData) (getter w))
        |> S.g [ SA.class [ "ew-charts-" ++ class ] ]



-- Hover Elements


viewHover :
    RenderData msg x y z datasets
    -> List (W.Chart.Internal.ChartElement msg x y z datasets)
    -> SC.Svg msg
viewHover (RenderData d) widgets =
    case d.attrs.hoverTarget of
        Just W.Chart.Internal.NearestX ->
            viewHoverX d widgets

        Just W.Chart.Internal.NearestPoint ->
            viewHoverNearest d widgets

        Nothing ->
            H.text ""


viewHoverX :
    W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.ChartElement msg x y z datasets)
    -> SC.Svg msg
viewHoverX d widgets =
    let
        bandwidth : Float
        bandwidth =
            Scale.bandwidth d.x.bandScale

        tooltipMargin : Float
        tooltipMargin =
            bandwidth * 0.5
    in
    d.points.byX
        |> Dict.values
        |> List.concatMap
            (\xData ->
                [ S.rect
                    [ SAP.x (Scale.convert d.x.bandScale xData.x.datum)
                    , SAP.y 0
                    , SAP.width bandwidth
                    , SAP.height d.spacings.chart.height
                    , SA.class [ "ew-charts--hover-target" ]
                    , Svg.Attributes.fill "transparent"
                    ]
                    []
                , S.g
                    [ SA.class [ "ew-charts--hover" ] ]
                    [ viewHoverData d widgets xData
                    , W.Chart.Tooltip.view d
                        xData.x.valueScaled
                        0.0
                        tooltipMargin
                        [ W.Chart.Tooltip.viewPoints d xData ]
                    ]
                ]
            )
        |> W.Chart.Internal.viewTranslateChart d.spacings


viewHoverNearest :
    W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.ChartElement msg x y z datasets)
    -> SC.Svg msg
viewHoverNearest d widgets =
    W.Chart.Internal.Voronoi.view
        (\( x, y ) hoverData ->
            S.g []
                [ viewHoverData d widgets hoverData
                , W.Chart.Tooltip.view d
                    x
                    y
                    8.0
                    [ W.Chart.Tooltip.viewPoints d hoverData ]
                ]
        )
        d


viewHoverData :
    W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.ChartElement msg x y z datasets)
    -> W.Chart.Internal.ChartPoint x y z
    -> SC.Svg msg
viewHoverData d widgets point =
    widgets
        |> List.filterMap
            (\(W.Chart.Internal.ChartElement w) ->
                w.hover
                    |> Maybe.andThen
                        (\hover ->
                            case hover of
                                W.Chart.Internal.HoverX fn ->
                                    fn d point.x
                                        |> List.singleton
                                        |> Just

                                W.Chart.Internal.HoverY fn ->
                                    Maybe.map
                                        (\yData ->
                                            List.map (fn d yData point.x) point.ys
                                        )
                                        d.y

                                W.Chart.Internal.HoverZ fn ->
                                    Maybe.map
                                        (\zData ->
                                            List.map (fn d zData point.x) point.zs
                                        )
                                        d.z

                                W.Chart.Internal.HoverYZ fn ->
                                    Maybe.map2
                                        (\yData zData ->
                                            fn d yData zData point
                                                |> List.singleton
                                        )
                                        d.y
                                        d.z
                        )
            )
        |> List.concat
        |> S.g []



-- Labels


viewLabels : RenderData msg x y z datasets -> SC.Svg msg
viewLabels (RenderData d) =
    S.g
        []
        [ d.attrs.yAxis.label
            |> W.Chart.Internal.maybeFilter (\_ -> W.Chart.Internal.isJust d.y)
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
            |> W.Chart.Internal.maybeFilter (\_ -> W.Chart.Internal.isJust d.z)
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


viewYGrid : RenderData msg x y z datasets -> SC.Svg msg
viewYGrid (RenderData d) =
    case ( d.attrs.yAxis.showGridLines, d.y ) of
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


viewXGrid : RenderData msg x y z datasets -> SC.Svg msg
viewXGrid (RenderData d) =
    case ( d.attrs.yAxis.showGridLines, d.y ) of
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


viewXAxis : RenderData msg x y z datasets -> SC.Svg msg
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


viewYAxis : RenderData msg x y z datasets -> SC.Svg msg
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


viewZAxis : RenderData msg x y z datasets -> SC.Svg msg
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



-- Styles


globalStyles : SC.Svg msg
globalStyles =
    H.node "style"
        []
        [ H.text ("""
            .ew-charts--svg,
            .ew-charts--tooltip-wrapper {
                overflow: visible;
            }

            .ew-charts.m--unfocus:hover .ew-charts-main {
                filter: grayscale(100%);
            }

            .ew-charts--hover {
                display: none;
                pointer-events: none;
            }
            .ew-charts--hover-target:hover + .ew-charts--hover {
                display: block;
            }

            .ew-charts--shadow {
                filter: drop-shadow(0 0 8px rgba(0, 0, 0, 0.5));
            }

            .ew-charts.m--debug .ew-charts--hover-target {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.1);
            }
            .ew-charts.m--debug .ew-charts--hover-target:hover {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.5);
            }

            .ew-charts--tooltip {
                display: flex;
                align-items: flex-end;
                justify-content: flex-start;
                box-sizing: border-box;
                font-family: var(--theme-font-text), sans-serif;
                font-size: 12px;
            }
            .ew-charts--tooltip.m--align-left {
                justify-content: flex-end;
            }
            .ew-charts--tooltip.m--align-top {
                align-items: flex-start;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--label,
            .ew-charts--tooltip-yz--list,
            .ew-charts--tooltip-yz--item {
                margin: 0;
                padding: 0;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz {
                padding: 4px;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--label {
                font-size: inherit;
            }

            .ew-charts--tooltip-yz--list {
                list-style-type: none;
            }

            .ew-charts--tooltip-yz {
                border-top: 1px solid """ ++ Theme.baseAuxWithAlpha 0.3 ++ """;
            }
            .ew-charts--tooltip-yz--label {
                padding-bottom: 4px;
            }

            .ew-charts--tooltip-yz--item {
                display: flex;
                justify-content: space-between;
                gap: 8px;
            }
            .ew-charts--tooltip-yz--item-label {}
            .ew-charts--tooltip-yz--item-value {}

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

            .ew-charts--animate-fade {
                animation: ew-charts--fade 0.4s ease-out forwards;
            }

            @keyframes ew-charts--fade {
                from {
                    opacity: 0;
                }
                to {
                    opacity: 1;
                }
            }

            .ew-charts--animate-h-clip {
                animation: ew-charts--h-clip 0.4s ease-out forwards;
            }

            @keyframes ew-charts--h-clip {
                from {
                    clip-path: rect(0 0 0 0);
                }
                to {
                    clip-path: rect(0 100% 100% 0);
                }
            }

            .ew-charts--animate-scale {
                transform: scale(0);
                animation: ew-charts--scale 0.2s ease-out forwards;
            }

            @keyframes ew-charts--scale {
                from {
                    transform: scale(0);
                }
                to {
                    transform: scale(1);
                }
            }

            /*
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
                fill: """ ++ Theme.baseAuxWithAlpha 0.03 ++ """;
            }
            .ew-charts--hover-rect.m--use-bars:hover,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-line,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-circle,
            .ew-charts--hover-rect:hover + g + .ew-charts--tooltip,
            .ew-charts--tooltip-trigger:hover + .ew-charts--tooltip {
                opacity: 1;
            }
            */
            """)
        ]
