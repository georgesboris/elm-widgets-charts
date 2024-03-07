module W.Chart exposing
    ( globalStyles, config, Config
    , withX, withY, withYList, withZ, withZList
    , view, Widget
    , width, ratio, padding, background, htmlAttrs, ChartAttribute
    , binPaddingInner, binPaddingOuter
    , withHover, noTooltip, groupByXY, DataPoint
    , axisLabel, defaultValue, format, noAxisLine, noGridLines, safety, stacked, stackedRelative, ticks, AxisAttribute
    , debug
    , onClickY, onClickYZ, onClickZ, onHoverY, onHoverYZ, onHoverZ
    )

{-|


# Setup

@docs globalStyles, config, Config


# Datasets

@docs withX, withY, withYList, withZ, withZList


# Widgets

@docs view, Widget


# Styles

@docs width, ratio, padding, background, htmlAttrs, ChartAttribute


# Bins

@docs binPaddingInner, binPaddingOuter


# Interaction

@docs withHover, noTooltip, groupByXY, DataPoint


# Axis

@docs axisLabel, defaultValue, format, noAxisLine, noGridLines, safety, stacked, stackedRelative, ticks, AxisAttribute


# Debugging

@docs debug

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
type alias Config msg x y z =
    W.Chart.Internal.Config msg x y z


{-| -}
type alias DataPoint a =
    { datum : a
    , label : String
    , color : String
    , value : Float
    }


{-| -}
type alias Widget msg x y z =
    W.Chart.Internal.Widget msg x y z


{-| -}
type alias ChartAttribute msg =
    Attr.Attr (W.Chart.Internal.Attributes msg)


{-| -}
type alias AxisAttribute =
    Attr.Attr W.Chart.Internal.AxisAttributes


{-| -}
type alias HoverAttribute msg x y z =
    Attr.Attr (W.Chart.Internal.HoverAttrs msg x y z)



-- Constants


lineStrokeWidth : Float
lineStrokeWidth =
    2


labelFontSize : Float
labelFontSize =
    13



-- Config


{-| -}
config : List (ChartAttribute msg) -> W.Chart.Internal.Config msg x y z
config =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs ->
            W.Chart.Internal.Config
                { attrs = attrs
                , hover = Nothing
                , yData = Nothing
                , zData = Nothing
                , xData = Nothing
                }
        )



-- Config : Attributes


{-| -}
width : Int -> ChartAttribute msg
width v =
    Attr.attr (\a -> { a | width = toFloat v })


{-| -}
ratio : Float -> ChartAttribute msg
ratio v =
    Attr.attr (\a -> { a | ratio = v })


{-| -}
padding : Int -> ChartAttribute msg
padding v =
    Attr.attr (\a -> { a | padding = toFloat v })


{-| -}
binPaddingOuter : Int -> ChartAttribute msg
binPaddingOuter v =
    Attr.attr (\a -> { a | binPaddingOuter = toFloat v })


{-| -}
binPaddingInner : Int -> ChartAttribute msg
binPaddingInner v =
    Attr.attr (\a -> { a | binPaddingInner = toFloat v })


{-| -}
background : String -> ChartAttribute msg
background v =
    Attr.attr (\a -> { a | background = v })


{-| -}
htmlAttrs : List (H.Attribute msg) -> ChartAttribute msg
htmlAttrs v =
    Attr.attr (\a -> { a | htmlAttributes = v })


{-| -}
debug : ChartAttribute msg
debug =
    Attr.attr (\a -> { a | debug = True })



-- Tooltips


defaultTooltipAttributes : W.Chart.Internal.HoverAttrs msg x y z
defaultTooltipAttributes =
    { nearest = False
    , tooltip = True
    , onClick = Nothing
    , onHover = Nothing
    , custom = []
    }


{-| -}
withHover : List (HoverAttribute msg x y z) -> Config msg x y z -> Config msg x y z
withHover =
    Attr.withAttrs defaultTooltipAttributes
        (\tooltipAttrs (W.Chart.Internal.Config cfg) ->
            W.Chart.Internal.Config { cfg | hover = Just tooltipAttrs }
        )


toDataPoint : W.Chart.Internal.DataPoint a -> DataPoint a
toDataPoint dp =
    { datum = dp.datum
    , label = "TODO"
    , color = "black"
    , value = dp.value
    }


{-| -}
onClickY : ({ x : DataPoint x, y : List (DataPoint y) } -> msg) -> HoverAttribute msg x y z
onClickY fn =
    Attr.attr
        (\a ->
            { a
                | onClick =
                    Just
                        (\point ->
                            fn
                                { x = toDataPoint point.x
                                , y = List.map toDataPoint point.ys
                                }
                        )
            }
        )


{-| -}
onClickZ : ({ x : DataPoint x, z : List (DataPoint z) } -> msg) -> HoverAttribute msg x y z
onClickZ fn =
    Attr.attr
        (\a ->
            { a
                | onClick =
                    Just
                        (\point ->
                            fn
                                { x = toDataPoint point.x
                                , z = List.map toDataPoint point.zs
                                }
                        )
            }
        )


{-| -}
onClickYZ : ({ x : DataPoint x, y : List (DataPoint y), z : List (DataPoint z) } -> msg) -> HoverAttribute msg x y z
onClickYZ fn =
    Attr.attr
        (\a ->
            { a
                | onClick =
                    Just
                        (\point ->
                            fn
                                { x = toDataPoint point.x
                                , y = List.map toDataPoint point.ys
                                , z = List.map toDataPoint point.zs
                                }
                        )
            }
        )


{-| -}
onHoverY : (Maybe { x : DataPoint x, y : List (DataPoint y) } -> msg) -> HoverAttribute msg x y z
onHoverY fn =
    Attr.attr
        (\a ->
            { a
                | onHover =
                    Just
                        (\maybePoint ->
                            maybePoint
                                |> Maybe.map
                                    (\point ->
                                        { x = toDataPoint point.x
                                        , y = List.map toDataPoint point.ys
                                        }
                                    )
                                |> fn
                        )
            }
        )


{-| -}
onHoverZ : (Maybe { x : DataPoint x, z : List (DataPoint z) } -> msg) -> HoverAttribute msg x y z
onHoverZ fn =
    Attr.attr
        (\a ->
            { a
                | onHover =
                    Just
                        (\maybePoint ->
                            maybePoint
                                |> Maybe.map
                                    (\point ->
                                        { x = toDataPoint point.x
                                        , z = List.map toDataPoint point.zs
                                        }
                                    )
                                |> fn
                        )
            }
        )


{-| -}
onHoverYZ : (Maybe { x : DataPoint x, y : List (DataPoint y), z : List (DataPoint z) } -> msg) -> HoverAttribute msg x y z
onHoverYZ fn =
    Attr.attr
        (\a ->
            { a
                | onHover =
                    Just
                        (\maybePoint ->
                            maybePoint
                                |> Maybe.map
                                    (\point ->
                                        { x = toDataPoint point.x
                                        , y = List.map toDataPoint point.ys
                                        , z = List.map toDataPoint point.zs
                                        }
                                    )
                                |> fn
                        )
            }
        )


{-| -}
noTooltip : HoverAttribute msg x y z
noTooltip =
    Attr.attr (\a -> { a | tooltip = False })


{-| -}
groupByXY : HoverAttribute msg x y z
groupByXY =
    Attr.attr (\a -> { a | nearest = True })



-- Datasets


{-| -}
withX :
    List AxisAttribute
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> Config msg x y z
    -> Config msg x y z
withX =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = axisAttrs }
                , hover = cfg.hover
                , yData = cfg.yData
                , zData = cfg.zData
                , xData =
                    Just
                        { data = axisData.data
                        , toLabel = axisData.toLabel
                        , toColor = \_ -> ""
                        , toValue = \_ _ -> Nothing
                        }
                }
        )


{-| -}
withY :
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x any z
    -> Config msg x () z
withY =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | yAxis = axisAttrs }
                , hover = Nothing
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


{-| -}
withYList :
    List AxisAttribute
    ->
        { data : List y
        , toLabel : y -> String
        , toColor : y -> String
        , toValue : y -> x -> Maybe Float
        }
    -> Config msg x y z
    -> Config msg x y z
withYList =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | yAxis = axisAttrs }
                , hover = cfg.hover
                , xData = cfg.xData
                , zData = cfg.zData
                , yData = Just axisData
                }
        )


{-| -}
withZ :
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> Config msg x y ()
    -> Config msg x y ()
withZ =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | zAxis = axisAttrs }
                , hover = cfg.hover
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


{-| -}
withZList :
    List AxisAttribute
    ->
        { data : List z
        , toLabel : z -> String
        , toColor : z -> String
        , toValue : z -> x -> Maybe Float
        }
    -> Config msg x y z
    -> Config msg x y z
withZList =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\axisAttrs axisData (W.Chart.Internal.Config cfg) ->
            let
                attrs : W.Chart.Internal.Attributes msg
                attrs =
                    cfg.attrs
            in
            W.Chart.Internal.Config
                { attrs = { attrs | zAxis = axisAttrs }
                , hover = cfg.hover
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



-- TODO: Log scales doesn't work yet. Need to investigate it more.
-- logarithmic : Float -> AxisAttribute
-- logarithmic basis =
--     Attr.attr (\attrs -> { attrs | scale = W.Chart.Internal.Logarithmic basis })


{-| -}
noAxisLine : AxisAttribute
noAxisLine =
    Attr.attr (\attrs -> { attrs | showAxis = False })


{-| -}
noGridLines : AxisAttribute
noGridLines =
    Attr.attr (\attrs -> { attrs | showGridLines = False })



-- View


{-| -}
view : List (W.Chart.Internal.Widget msg x y z) -> Config msg x y z -> H.Html msg
view widgets (W.Chart.Internal.Config cfg) =
    cfg.xData
        |> Maybe.map
            (\xData ->
                let
                    renderData : RenderData msg x y z
                    renderData =
                        W.Chart.Internal.toRenderData cfg xData

                    (W.Chart.Internal.RenderData d) =
                        renderData
                in
                H.div
                    [ HA.class "ew-charts"
                    , HA.classList
                        [ ( "m--unfocus", True || cfg.hover /= Nothing )
                        , ( "m--debug", d.attrs.debug )
                        ]
                    ]
                    [ Svg.svg
                        [ SA.viewBox 0 0 d.spacings.canvas.width d.spacings.canvas.height
                        , SA.class [ "ew-charts--svg" ]
                        ]
                        [ -- Grid
                          W.Chart.Internal.viewTranslateChart d.spacings
                            [ viewYGrid renderData
                            , viewXGrid renderData
                            ]

                        -- Labels
                        , viewLabels renderData

                        -- Axis
                        , viewXAxis renderData
                        , viewYAxis renderData
                        , viewZAxis renderData

                        -- Elements & Hover
                        , W.Chart.Internal.viewTranslateChart d.spacings
                            [ viewWidgets "bg" .background renderData widgets
                            , viewWidgets "main" .main renderData widgets
                            , viewWidgets "fg" .foreground renderData widgets
                            , viewHover cfg.hover renderData widgets
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (H.text "")



-- Static Elements


viewWidgets :
    String
    -> (W.Chart.Internal.WidgetData msg x y z -> Maybe (RenderData msg x y z -> Svg.Svg msg))
    -> RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z)
    -> SC.Svg msg
viewWidgets class getter renderData widgets =
    widgets
        |> List.filterMap (\(W.Chart.Internal.Widget w) -> Maybe.map (\el_ -> el_ renderData) (getter w))
        |> S.g [ SA.class [ "ew-charts--" ++ class ] ]



-- Hover Elements


viewHover :
    Maybe (W.Chart.Internal.HoverAttrs msg x y z)
    -> RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z)
    -> SC.Svg msg
viewHover maybeHoverAttrs (RenderData d) widgets =
    maybeHoverAttrs
        |> Maybe.map
            (\hoverAttrs ->
                if hoverAttrs.nearest then
                    viewHoverNearest hoverAttrs d widgets

                else
                    viewHoverX hoverAttrs d widgets
            )
        |> Maybe.withDefault (H.text "")


viewHoverX :
    W.Chart.Internal.HoverAttrs msg x y z
    -> W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.Widget msg x y z)
    -> SC.Svg msg
viewHoverX hoverAttrs d widgets =
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
                    , if hoverAttrs.tooltip then
                        W.Chart.Tooltip.view d
                            xData.x.valueScaled
                            0.0
                            tooltipMargin
                            [ W.Chart.Tooltip.viewPoints d xData ]

                      else
                        H.text ""
                    ]
                ]
            )
        |> S.g []


viewHoverNearest :
    W.Chart.Internal.HoverAttrs msg x y z
    -> W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.Widget msg x y z)
    -> SC.Svg msg
viewHoverNearest hoverAttrs d widgets =
    W.Chart.Internal.Voronoi.view
        (\( x, y ) hoverData ->
            S.g []
                [ viewHoverData d widgets hoverData
                , if hoverAttrs.tooltip then
                    W.Chart.Tooltip.view d
                        x
                        y
                        8.0
                        [ W.Chart.Tooltip.viewPoints d hoverData ]

                  else
                    H.text ""
                ]
        )
        d


viewHoverData :
    W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.Widget msg x y z)
    -> W.Chart.Internal.ChartPoint x y z
    -> SC.Svg msg
viewHoverData d widgets point =
    widgets
        |> List.filterMap
            (\(W.Chart.Internal.Widget w) ->
                w.hover
                    |> Maybe.andThen
                        (\hover ->
                            case hover of
                                W.Chart.Internal.HoverX fn ->
                                    fn d point.x
                                        |> Just

                                W.Chart.Internal.HoverY fn ->
                                    Maybe.map
                                        (\yData ->
                                            fn d yData point.x point.ys
                                        )
                                        d.y

                                W.Chart.Internal.HoverZ fn ->
                                    Maybe.map
                                        (\zData ->
                                            fn d zData point.x point.zs
                                        )
                                        d.z

                                W.Chart.Internal.HoverYZ fn ->
                                    Maybe.map2
                                        (\yData zData ->
                                            fn d yData zData point
                                        )
                                        d.y
                                        d.z
                        )
            )
        |> S.g []



-- Labels


viewLabels : RenderData msg x y z -> SC.Svg msg
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


viewYGrid : RenderData msg x y z -> SC.Svg msg
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
                |> S.g []

        _ ->
            H.text ""


viewXGrid : RenderData msg x y z -> SC.Svg msg
viewXGrid (RenderData d) =
    case ( d.attrs.yAxis.showGridLines, d.y ) of
        ( True, Just _ ) ->
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
                |> S.g []

        _ ->
            H.text ""


viewXAxis : RenderData msg x y z -> SC.Svg msg
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


viewYAxis : RenderData msg x y z -> SC.Svg msg
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


viewZAxis : RenderData msg x y z -> SC.Svg msg
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


{-| -}
globalStyles : SC.Svg msg
globalStyles =
    H.node "style"
        []
        [ H.text ("""
            /* Prevent Tooltip Clipping */

            .ew-charts--svg,
            .ew-charts--tooltip-wrapper {
                overflow: visible;
            }

            /* Unfocus */

            .ew-charts.m--unfocus:hover .ew-charts--main {
                filter: grayscale(0%);
            }

            /* Hover */

            .ew-charts--hover {
                display: none;
                pointer-events: none;
            }
            .ew-charts--hover-target:hover + .ew-charts--hover {
                display: block;
            }

            /* Debug */

            .ew-charts.m--debug .ew-charts--hover-target {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.1);
            }
            .ew-charts.m--debug .ew-charts--hover-target:hover {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.5);
            }

            /* Tooltip */

            .ew-charts--tooltip {
                display: flex;
                align-items: flex-end;
                justify-content: flex-start;
                box-sizing: border-box;
                font-family: var(--theme-font-text), sans-serif;
                font-size: 12px;
                line-height: 1;
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
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz {
                padding: 4px;
            }

            .ew-charts--tooltip-x {
                font-weight: normal;
                color: """ ++ Theme.baseAux ++ """;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--label {
                font-size: inherit;
            }

            .ew-charts--tooltip-yz {
                border-top: 1px solid """ ++ Theme.baseAuxWithAlpha 0.1 ++ """;
                padding: 0 4px;
            }

            .ew-charts--tooltip-yz--label {
                padding: 4px 0;
            }

            .ew-charts--tooltip-yz--list {
                list-style-type: none;
                margin: 0;
                padding: 0;
            }

            .ew-charts--tooltip-yz--item {
                display: flex;
                align-items: center;
                justify-content: space-between;
                gap: 4px;
                padding: 2px 0;
                margin: 0;
            }
            .ew-charts--tooltip-yz--item-color {
                height: 8px;
                width: 8px;
                border-radius: 2px;
            }
            .ew-charts--tooltip-yz--item-label {
                flex-grow: 1;
                padding: 0 8px 0 2px;
            }
            .ew-charts--tooltip-yz--item-value {}

            /* Axis & Labels */

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

            /* Animations */

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

            .ew-charts--animate-scale-z {
                transform: scale(1,0);
                animation: ew-charts--scale-z 0.2s ease-out forwards;
            }

            @keyframes ew-charts--scale-z {
                from {
                    transform: scale(1,0);
                }
                to {
                    transform: scale(1,1);
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
