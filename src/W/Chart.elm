module W.Chart exposing
    ( globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config
    , xAxis, axis, axisList
    , axisLabel, format, palette, defaultValue, noAxisLine, noGridLines, safety, stacked, distribution, ticks, AxisAttribute
    , width, ratio, padding, background, htmlAttrs, ChartAttribute
    , view, WidgetX, WidgetXY, WidgetXYZ, Widget
    , withActive, withHover, noTooltip, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ
    , Point, RenderData, RenderDatum, PointData
    , debug
    )

{-|


# Setup

@docs globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config


# Axis

@docs xAxis, axis, axisList


# Axis Attributes

@docs axisLabel, format, palette, defaultValue, noAxisLine, noGridLines, safety, stacked, distribution, ticks, AxisAttribute


# Styles

@docs width, ratio, padding, background, htmlAttrs, ChartAttribute


# Widgets

@docs view, WidgetX, WidgetXY, WidgetXYZ, Widget


# Interaction

@docs withActive, withHover, noTooltip, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ


# Tmp

@docs Point, RenderData, RenderDatum, PointData


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
import Svg.Events
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Colors
import W.Chart.Internal
import W.Chart.Internal.Voronoi
import W.Chart.Tooltip
import W.Svg.Attributes


{-| -}
type alias Config msg x y z point =
    W.Chart.Internal.Config msg x y z point


{-| -}
type alias ConfigX msg x =
    W.Chart.Internal.Config msg x () () { x : Point x }


{-| -}
type alias ConfigXY msg x y =
    W.Chart.Internal.Config msg x y () { x : Point x, y : List (Point y) }


{-| -}
type alias ConfigXYZ msg x y z =
    W.Chart.Internal.Config msg x y z { x : Point x, y : List (Point y), z : List (Point z) }


{-| -}
type alias PointX x =
    W.Chart.Internal.ChartPointData
        { x : Point x
        }


{-| -}
type alias PointXY x y =
    W.Chart.Internal.ChartPointData
        { x : Point x
        , y : List (Point y)
        }


{-| -}
type alias PointXYZ x y z =
    W.Chart.Internal.ChartPointData
        { x : Point x
        , y : List (Point y)
        , z : List (Point z)
        }


{-| -}
type alias Point a =
    W.Chart.Internal.DataPoint a


{-| -}
type alias PointData point =
    W.Chart.Internal.ChartPointData point


{-| -}
type alias Widget msg x y z point =
    W.Chart.Internal.Widget msg x y z point


{-| -}
type alias WidgetX msg x y z a =
    Widget msg x y z { a | x : Point x }


{-| -}
type alias WidgetXY msg x y z a =
    Widget msg x y z { a | x : Point x, y : List (Point y) }


{-| -}
type alias WidgetXYZ msg x y z a =
    Widget msg x y z { a | x : Point x, y : List (Point y), z : List (Point z) }


{-| -}
type alias ChartAttribute msg =
    Attr.Attr (W.Chart.Internal.Attributes msg)


{-| -}
type alias AxisConfigX x =
    AxisConfig x x { xAxis : Bool }


{-| -}
type alias AxisConfigYZ x y =
    AxisConfig x y {}


{-| -}
type AxisConfig x a constraint
    = AxisConfig
        W.Chart.Internal.AxisAttributes
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }


{-| -}
type alias AxisAttribute =
    Attr.Attr W.Chart.Internal.AxisAttributes


{-| -}
type alias HoverAttribute msg g =
    Attr.Attr (W.Chart.Internal.HoverAttrs msg g)


{-| -}
type alias Context x y z =
    W.Chart.Internal.Context x y z


{-| -}
type alias RenderData point =
    W.Chart.Internal.ChartPointData point


{-| -}
type alias RenderDatum =
    W.Chart.Internal.RenderDatum



-- Constants


lineStrokeWidth : Float
lineStrokeWidth =
    2


labelFontSize : Float
labelFontSize =
    13



-- Config


{-| -}
fromX :
    List (ChartAttribute msg)
    -> { x : AxisConfigX x }
    -> ConfigX msg x
fromX =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs }
                , toPoint = \point -> { x = point.x }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Nothing
                , zData = Nothing
                }
        )


{-| -}
fromXY :
    List (ChartAttribute msg)
    ->
        { x : AxisConfigX x
        , y : AxisConfigYZ x y
        }
    -> ConfigXY msg x y
fromXY =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x

                (AxisConfig yAttrs yData) =
                    props.y
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs, yAxis = yAttrs }
                , toPoint =
                    \point ->
                        { x = point.x
                        , y = point.ys
                        }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Just yData
                , zData = Nothing
                }
        )


{-| -}
fromXYZ :
    List (ChartAttribute msg)
    ->
        { x : AxisConfigX x
        , y : AxisConfigYZ x y
        , z : AxisConfigYZ x z
        }
    -> ConfigXYZ msg x y z
fromXYZ =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x

                (AxisConfig yAttrs yData) =
                    props.y

                (AxisConfig zAttrs zData) =
                    props.z
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs, yAxis = yAttrs, zAxis = zAttrs }
                , toPoint = \point -> { x = point.x, y = point.ys, z = point.zs }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Just yData
                , zData = Just zData
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


{-| -}
withActive : Maybe (W.Chart.Internal.ChartPointData point) -> Config msg x y z point -> Config msg x y z point
withActive v (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config { cfg | activePoint = v }


{-| -}
withHover : List (HoverAttribute msg point) -> Config msg x y z point -> Config msg x y z point
withHover =
    Attr.withAttrs
        { nearest = False
        , tooltip = True
        , onClick = Nothing
        , onMouseEnter = Nothing
        , onMouseLeave = Nothing
        , custom = []
        }
        (\tooltipAttrs (W.Chart.Internal.Config cfg) ->
            W.Chart.Internal.Config { cfg | hover = Just tooltipAttrs }
        )


{-| -}
onClick : (W.Chart.Internal.ChartPointData point -> msg) -> HoverAttribute msg point
onClick fn =
    Attr.attr (\a -> { a | onClick = Just fn })


{-| -}
onMouseEnter : (W.Chart.Internal.ChartPointData point -> msg) -> HoverAttribute msg point
onMouseEnter fn =
    Attr.attr (\a -> { a | onMouseEnter = Just fn })


{-| -}
onMouseLeave : (W.Chart.Internal.ChartPointData point -> msg) -> HoverAttribute msg point
onMouseLeave fn =
    Attr.attr (\a -> { a | onMouseLeave = Just fn })


{-| -}
noTooltip : HoverAttribute msg point
noTooltip =
    Attr.attr (\a -> { a | tooltip = False })


{-| -}
groupByXY : HoverAttribute msg point
groupByXY =
    Attr.attr (\a -> { a | nearest = True })



-- Axis


{-| -}
xAxis :
    List AxisAttribute
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> AxisConfigX x
xAxis =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props ->
            AxisConfig
                attrs
                { data = props.data
                , toLabel = props.toLabel
                , toColor = \_ -> ""
                , toValue = \_ _ -> Nothing
                }
        )


{-| -}
axis :
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> AxisConfigYZ x String
axis =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props ->
            AxisConfig
                attrs
                { data = [ props.label ]
                , toLabel = \_ -> props.label
                , toColor = \_ -> props.color
                , toValue = \_ -> props.toValue
                }
        )


{-| -}
axisList :
    List AxisAttribute
    ->
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }
    -> AxisConfigYZ x a
axisList =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props -> AxisConfig attrs props)



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
palette : W.Chart.Colors.Palette -> AxisAttribute
palette v =
    Attr.attr (\attrs -> { attrs | palette = v })


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
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Stacked })


{-| -}
distribution : AxisAttribute
distribution =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Distribution })



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
    Attr.attr (\attrs -> { attrs | showGrid = False })



-- View


{-| -}
view : List (W.Chart.Internal.Widget msg x y z point) -> Config msg x y z point -> H.Html msg
view widgets (W.Chart.Internal.Config cfg) =
    cfg.xData
        |> Maybe.map
            (\xData ->
                let
                    renderData : W.Chart.Internal.RenderData msg x y z
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
                            , viewXGrid d.ctx
                            ]

                        -- Labels
                        , viewLabels renderData

                        -- Axis
                        , viewYAxis renderData
                        , viewZAxis renderData

                        -- Elements & Hover
                        , W.Chart.Internal.viewTranslateChart d.spacings
                            [ viewXAxis d.ctx
                            , viewWidgets "bg" .background renderData widgets
                            , viewWidgets "main" .main renderData widgets
                            , viewWidgets "fg" .foreground renderData widgets
                            , viewHover cfg renderData widgets
                            , viewActive cfg renderData widgets
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (H.text "")



-- Static Elements


viewWidgets :
    String
    -> (W.Chart.Internal.WidgetData msg x y z point -> Maybe (Context x y z -> Svg.Svg msg))
    -> W.Chart.Internal.RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewWidgets class getter (W.Chart.Internal.RenderData { ctx }) widgets =
    widgets
        |> List.filterMap (\(W.Chart.Internal.Widget w) -> Maybe.map (\el_ -> el_ ctx) (getter w))
        |> S.g [ SA.class [ "ew-charts--" ++ class ] ]


viewActive :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewActive cfg (W.Chart.Internal.RenderData d) widgets =
    cfg.activePoint
        |> Maybe.map
            (\pointData ->
                viewChartPoint
                    d
                    pointData
                    widgets
                    True
                    ( pointData.pos.x, pointData.pos.y )
                    |> S.g [ Svg.Attributes.class "ew-charts--active" ]
            )
        |> Maybe.withDefault (H.text "")



-- Hover Elements


viewHover :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHover cfg (W.Chart.Internal.RenderData d) widgets =
    cfg.hover
        |> Maybe.map
            (\hoverAttrs_ ->
                let
                    hoverAttrs : W.Chart.Internal.HoverAttrs msg point
                    hoverAttrs =
                        if cfg.activePoint == Nothing then
                            hoverAttrs_

                        else
                            { hoverAttrs_ | tooltip = False }
                in
                if hoverAttrs.nearest then
                    viewHoverNearest cfg hoverAttrs d widgets

                else
                    viewHoverX cfg hoverAttrs d widgets
            )
        |> Maybe.withDefault (H.text "")


viewHoverX :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverX cfg hoverAttrs d widgets =
    d.points.byX
        |> Dict.values
        |> List.concatMap
            (\xData ->
                let
                    point : W.Chart.Internal.ChartPointData point
                    point =
                        { point = cfg.toPoint xData
                        , pos = xData.pos
                        , x = xData.xRender
                        , y = xData.yRender
                        , z = xData.zRender
                        }
                in
                [ S.rect
                    ([ SAP.x xData.x.render.valueStart
                     , SAP.y 0
                     , SAP.width (Scale.bandwidth d.x.bandScale)
                     , SAP.height d.spacings.chart.height
                     ]
                        ++ viewHoverAttrs hoverAttrs point
                    )
                    []
                , viewHoverContent
                    hoverAttrs
                    d
                    point
                    widgets
                    ( xData.x.render.valueScaled, 0.0 )
                ]
            )
        |> S.g []


viewHoverNearest :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.RenderDataFull msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverNearest cfg hoverAttrs d widgets =
    W.Chart.Internal.Voronoi.view
        (\( x, y ) hoverData polygon ->
            let
                point : W.Chart.Internal.ChartPointData point
                point =
                    { point = cfg.toPoint hoverData
                    , pos = hoverData.pos
                    , x = hoverData.xRender
                    , y = hoverData.yRender
                    , z = hoverData.zRender
                    }
            in
            [ polygon (viewHoverAttrs hoverAttrs point)
            , viewHoverContent
                hoverAttrs
                d
                point
                widgets
                ( x, y )
            ]
        )
        d


viewHoverContent :
    W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.RenderDataFull msg x y z
    -> PointData point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> ( Float, Float )
    -> SC.Svg msg
viewHoverContent hoverAttrs d pointData widgets ( x, y ) =
    S.g
        [ SA.class [ "ew-charts--hover" ] ]
        ((if d.attrs.debug then
            S.circle
                [ SAP.cx x
                , SAP.cy y
                , SAP.r 2.0
                , Svg.Attributes.fill "red"
                ]
                []

          else
            H.text ""
         )
            :: viewChartPoint
                d
                pointData
                widgets
                hoverAttrs.tooltip
                ( x, y )
        )


viewChartPoint :
    W.Chart.Internal.RenderDataFull msg x y z
    -> PointData point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> Bool
    -> ( Float, Float )
    -> List (SC.Svg msg)
viewChartPoint d pointData widgets showTooltip ( x, y ) =
    let
        bandwidth : Float
        bandwidth =
            Scale.bandwidth d.x.bandScale

        tooltipMargin : Float
        tooltipMargin =
            bandwidth * 0.5
    in
    [ viewHoverWidgets d.ctx pointData widgets
    , if showTooltip then
        W.Chart.Tooltip.view d
            x
            y
            tooltipMargin
            [ W.Chart.Tooltip.viewPoints d pointData ]

      else
        H.text ""
    ]


viewHoverAttrs : W.Chart.Internal.HoverAttrs msg point -> W.Chart.Internal.ChartPointData point -> List (Svg.Attribute msg)
viewHoverAttrs hoverAttrs point =
    [ SA.class [ "ew-charts--hover-target" ]
    , Svg.Attributes.fill "transparent"
    , W.Svg.Attributes.maybe hoverAttrs.onClick (\fn -> Svg.Events.onClick (fn point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseEnter (\fn -> Svg.Events.onMouseOver (fn point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseLeave (\fn -> Svg.Events.onMouseOut (fn point))
    ]


viewHoverWidgets :
    Context x y z
    -> PointData point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverWidgets ctx point widgets =
    widgets
        |> List.filterMap
            (\(W.Chart.Internal.Widget w) ->
                w.hover
                    |> Maybe.map (\fn -> fn ctx point)
            )
        |> W.Chart.Internal.maybeIf (not << List.isEmpty)
        |> Maybe.map (S.g [])
        |> Maybe.withDefault (H.text "")



-- Labels


viewLabels : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewLabels (W.Chart.Internal.RenderData d) =
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
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        ]



--  Axes & Lines


viewYGrid : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewYGrid (W.Chart.Internal.RenderData d) =
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
                |> S.g []

        _ ->
            H.text ""


viewXGrid : Context x y z -> SC.Svg msg
viewXGrid ctx =
    if ctx.x.showGrid then
        let
            step : Int
            step =
                List.length ctx.x.data // ctx.x.ticks
        in
        ctx.points.byX
            |> Dict.values
            |> List.indexedMap
                (\index { x } ->
                    if modBy step index == 0 then
                        S.line
                            [ SAP.x1 x.render.valueScaled
                            , SAP.x2 x.render.valueScaled
                            , SAP.y1 0
                            , SAP.y2 ctx.y.range
                            , SA.strokeWidth (ST.px 1.0)
                            , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                            ]
                            []

                    else
                        H.text ""
                )
            |> S.g []

    else
        H.text ""


viewXAxis : Context x y z -> SC.Svg msg
viewXAxis ctx =
    if ctx.x.showAxis then
        let
            step : Int
            step =
                List.length ctx.x.data // ctx.x.ticks
        in
        W.Chart.Internal.viewTranslate
            { x = 0
            , y = ctx.y.range
            }
            [ S.line
                [ SAP.x1 0
                , SAP.x2 ctx.x.range
                , SAP.y1 0
                , SAP.y2 0
                , SAP.strokeWidth 2
                , Svg.Attributes.stroke Theme.baseAux
                ]
                []
            , ctx.x.data
                |> List.indexedMap
                    (\index x ->
                        if modBy step index == 0 then
                            S.text_
                                [ SA.textAnchor ST.AnchorMiddle
                                , SAP.y (labelFontSize + 8)
                                , SAP.x (Scale.convert ctx.x.scale x)
                                , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.6)
                                , SAP.fontSize labelFontSize
                                ]
                                [ SC.text (ctx.x.format x) ]

                        else
                            H.text ""
                    )
                |> S.g []
            ]

    else
        H.text ""


viewYAxis : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewYAxis (W.Chart.Internal.RenderData d) =
    case ( d.attrs.yAxis.showAxis, d.y ) of
        ( True, Just _ ) ->
            W.Chart.Internal.viewTranslate
                { x = d.spacings.padding.left
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "ew-charts--y-axis" ] ]
                    [ viewAxis
                        Axis.left
                        { ticks = d.ctx.y.ticks
                        , format = d.ctx.y.format
                        , scale = d.ctx.y.scale
                        }
                    ]
                ]

        _ ->
            H.text ""


viewZAxis : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewZAxis (W.Chart.Internal.RenderData d) =
    case ( d.attrs.zAxis.showAxis, d.z ) of
        ( True, Just _ ) ->
            W.Chart.Internal.viewTranslate
                { x = d.spacings.padding.left + d.spacings.chart.width
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "ew-charts--z-axis" ] ]
                    [ viewAxis
                        Axis.right
                        { ticks = d.ctx.z.ticks
                        , format = d.ctx.z.format
                        , scale = d.ctx.z.scale
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
viewAxis axis_ props =
    axis_
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
            /* Basics */

            .ew-charts text {
                font-family: var(--theme-font-text), sans-serif;
                font-size: 13px;
            }

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

            .ew-charts--active,
            .ew-charts--hover {
                pointer-events: none;
            }
            .ew-charts--hover {
                display: none;
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

            .ew-charts--tooltip-x {
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
                padding: 4px 0 0;
            }

            .ew-charts--tooltip-yz--list {
                list-style-type: none;
                margin: 0;
                padding: 4px 0;
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
                flex-shrink: 0;
                height: 8px;
                width: 8px;
                border-radius: 2px;
            }
            .ew-charts--tooltip-yz--item-label {
                flex-grow: 1;
                padding: 0 8px 0 2px;
            }
            .ew-charts--tooltip-yz--item-value {
                flex-shrink: 0;
            }

            /* Axis & Labels */

            .ew-charts .tick text {
                fill: """ ++ Theme.baseAux ++ """;
                font-family: var(--theme-font-text), sans-serif;
                font-size: 12px;
            }

            .ew-charts--x-axis path.domain,
            .ew-charts--y-axis path.domain,
            .ew-charts--z-axis path.domain {
                stroke: """ ++ Theme.baseAux ++ """;
                stroke-width: """ ++ String.fromFloat lineStrokeWidth ++ """px;
            }

            .ew-charts--x-axis .tick line,
            .ew-charts--y-axis .tick line,
            .ew-charts--z-axis .tick line {
                stroke: """ ++ Theme.baseAux ++ """;
            }

            .ew-charts--y-axis path.domain,
            .ew-charts--y-axis .tick line {
                display: none;
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
