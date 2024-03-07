module W.Chart.Bar exposing (yBars, zBars, yzBars)

{-|

@docs yBars, zBars, yzBars

-}

import Dict
import Html as H
import Scale
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes


{-| -}
yBars : W.Chart.Internal.Widget msg x y z
yBars =
    W.Chart.Widget.fromY (viewBars toBarsDataY .y .y .ys (.attrs >> .yAxis))
        |> W.Chart.Widget.withHoverY
            (\d yData xPoint yPoints ->
                viewHover d xPoint (Just ( yData, yPoints )) Nothing
            )


{-| -}
zBars : W.Chart.Internal.Widget msg x y z
zBars =
    W.Chart.Widget.fromZ (viewBars toBarsDataZ .z .z .zs (.attrs >> .zAxis))
        |> W.Chart.Widget.withHoverZ
            (\d zData xPoint zPoints ->
                viewHover d xPoint Nothing (Just ( zData, zPoints ))
            )


{-| -}
yzBars : W.Chart.Internal.Widget msg x y z
yzBars =
    W.Chart.Widget.fromYZ viewMixedBars
        |> W.Chart.Widget.withHoverYZ
            (\d yData zData point ->
                viewHover d
                    point.x
                    (Just ( yData, point.ys ))
                    (Just ( zData, point.zs ))
            )



-- Helpers


type alias AxisData a =
    { zero : Float
    , vScaled : W.Chart.Internal.DataPoint a -> Float
    , xOffset : a -> Float
    }


type alias BarsData y z =
    { barWidth : Float
    , y : AxisData y
    , z : AxisData z
    }


toBarsDataY : W.Chart.Internal.RenderDataFull msg x y z -> BarsData y z
toBarsDataY renderData =
    toBarsData renderData renderData.y Nothing


toBarsDataZ : W.Chart.Internal.RenderDataFull msg x y z -> BarsData y z
toBarsDataZ renderData =
    toBarsData renderData Nothing renderData.z


toBarsData :
    W.Chart.Internal.RenderDataFull msg x y z
    -> Maybe (W.Chart.Internal.RenderDataYZ x y)
    -> Maybe (W.Chart.Internal.RenderDataYZ x z)
    -> BarsData y z
toBarsData renderData maybeYData maybeZData =
    let
        yStacked : Bool
        yStacked =
            renderData.attrs.yAxis.stackType /= W.Chart.Internal.NoStack

        zStacked : Bool
        zStacked =
            renderData.attrs.zAxis.stackType /= W.Chart.Internal.NoStack

        yBinsCount : Int
        yBinsCount =
            maybeYData
                |> Maybe.map
                    (\yData ->
                        if yStacked then
                            1

                        else
                            List.length yData.data
                    )
                |> Maybe.withDefault 0

        zBinsCount : Int
        zBinsCount =
            maybeZData
                |> Maybe.map
                    (\zData ->
                        if zStacked then
                            1

                        else
                            List.length zData.data
                    )
                |> Maybe.withDefault 0

        binsCount : Int
        binsCount =
            yBinsCount + zBinsCount

        binScale : Scale.BandScale Int
        binScale =
            Scale.band
                { paddingInner = renderData.attrs.binPaddingInner
                , paddingOuter = renderData.attrs.binPaddingOuter
                , align = 0.5
                }
                ( 0, Scale.bandwidth renderData.x.bandScale )
                (List.range 0 (binsCount - 1))

        yZero : Float
        yZero =
            maybeYData
                |> Maybe.map (\yData -> Scale.convert yData.scale 0.0)
                |> Maybe.withDefault 0.0

        yxOffsets : Dict.Dict String Float
        yxOffsets =
            maybeYData
                |> Maybe.map
                    (\yData ->
                        yData.data
                            |> List.indexedMap
                                (\index y ->
                                    ( yData.toLabel y
                                    , if yStacked then
                                        Scale.convert binScale 0

                                      else
                                        Scale.convert binScale index
                                    )
                                )
                            |> Dict.fromList
                    )
                |> Maybe.withDefault Dict.empty

        zZero : Float
        zZero =
            maybeZData
                |> Maybe.map (\zData -> Scale.convert zData.scale 0.0)
                |> Maybe.withDefault 0.0

        zxOffsets : Dict.Dict String Float
        zxOffsets =
            maybeZData
                |> Maybe.map
                    (\zData ->
                        zData.data
                            |> List.indexedMap
                                (\index z ->
                                    ( zData.toLabel z
                                    , if zStacked then
                                        Scale.convert binScale yBinsCount

                                      else
                                        Scale.convert binScale (index + yBinsCount)
                                    )
                                )
                            |> Dict.fromList
                    )
                |> Maybe.withDefault Dict.empty
    in
    { barWidth = Scale.bandwidth binScale
    , y =
        { zero = yZero
        , vScaled =
            \vPoint ->
                if renderData.attrs.yAxis.stackType == W.Chart.Internal.NoStack && vPoint.valueStart > yZero then
                    yZero

                else
                    vPoint.valueStart
        , xOffset =
            maybeYData
                |> Maybe.map
                    (\yData y ->
                        Dict.get (yData.toLabel y) yxOffsets
                            |> Maybe.withDefault 0.0
                    )
                |> Maybe.withDefault (\_ -> 0.0)
        }
    , z =
        { zero = zZero
        , vScaled =
            \vPoint ->
                if renderData.attrs.zAxis.stackType == W.Chart.Internal.NoStack && vPoint.valueStart > zZero then
                    zZero

                else
                    vPoint.valueStart
        , xOffset =
            maybeZData
                |> Maybe.map
                    (\zData z ->
                        Dict.get (zData.toLabel z) zxOffsets
                            |> Maybe.withDefault 0.0
                    )
                |> Maybe.withDefault (\_ -> 0.0)
        }
    }


viewBar :
    { isHover : Bool
    , renderData : W.Chart.Internal.RenderDataFull msg x y z
    , barsData : BarsData y z
    , axisData : AxisData a
    , xPoint : W.Chart.Internal.DataPoint x
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    }
    -> W.Chart.Internal.DataPoint a
    -> SC.Svg msg
viewBar props yzPoint =
    let
        color : String
        color =
            props.yzData.toColor yzPoint.datum

        x : Float
        x =
            props.xPoint.valueStart + props.axisData.xOffset yzPoint.datum

        bar : SC.Svg msg
        bar =
            S.rect
                [ Svg.Attributes.fill color
                , SAP.x x
                , SAP.y (props.axisData.vScaled yzPoint)
                , SAP.width props.barsData.barWidth
                , SAP.height (abs (yzPoint.valueStart - yzPoint.valueEnd))
                , W.Svg.Attributes.cond props.isHover (Svg.Attributes.stroke Theme.baseBackground)
                , W.Svg.Attributes.cond props.isHover (SAP.strokeWidth 2)
                , W.Svg.Attributes.cond props.isHover
                    (W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 4.0
                        , color = color
                        }
                    )
                ]
                []
    in
    if props.isHover then
        bar

    else
        S.g
            [ W.Chart.Internal.attrAnimationDelayX props.renderData.spacings x
            , W.Chart.Internal.attrTransformOrigin x props.barsData.y.zero
            , Svg.Attributes.class "ew-charts--animate-scale-z"
            ]
            [ bar ]


viewHover :
    W.Chart.Internal.RenderDataFull msg x y z
    -> W.Chart.Internal.DataPoint x
    -> Maybe ( W.Chart.Internal.RenderDataYZ x y, List (W.Chart.Internal.DataPoint y) )
    -> Maybe ( W.Chart.Internal.RenderDataYZ x z, List (W.Chart.Internal.DataPoint z) )
    -> SC.Svg msg
viewHover renderData xPoint maybeYPoints maybeZPoints =
    let
        barsData : BarsData y z
        barsData =
            toBarsData
                renderData
                (Maybe.map Tuple.first maybeYPoints)
                (Maybe.map Tuple.first maybeZPoints)
    in
    S.g []
        [ maybeYPoints
            |> Maybe.map
                (\( yData, yPoints ) ->
                    viewBinBars
                        { isHover = True
                        , renderData = renderData
                        , barsData = barsData
                        , axisData = barsData.y
                        , xPoint = xPoint
                        , yzData = yData
                        , yzConfig = renderData.attrs.yAxis
                        }
                        yPoints
                )
            |> Maybe.withDefault (H.text "")
        , maybeZPoints
            |> Maybe.map
                (\( zData, zPoints ) ->
                    viewBinBars
                        { isHover = True
                        , renderData = renderData
                        , barsData = barsData
                        , axisData = barsData.z
                        , xPoint = xPoint
                        , yzData = zData
                        , yzConfig = renderData.attrs.zAxis
                        }
                        zPoints
                )
            |> Maybe.withDefault (H.text "")
        ]


viewBinBars :
    { isHover : Bool
    , renderData : W.Chart.Internal.RenderDataFull msg x y z
    , barsData : BarsData y z
    , axisData : AxisData a
    , xPoint : W.Chart.Internal.DataPoint x
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    }
    -> List (W.Chart.Internal.DataPoint a)
    -> SC.Svg msg
viewBinBars props yzPoints =
    yzPoints
        |> List.map (viewBar props)
        |> S.g []


viewBars :
    (W.Chart.Internal.RenderDataFull msg x y z -> BarsData y z)
    -> (BarsData y z -> AxisData a)
    -> (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    -> (W.Chart.Internal.ChartPoint x y z -> List (W.Chart.Internal.DataPoint a))
    -> (W.Chart.Internal.RenderDataFull msg x y z -> W.Chart.Internal.AxisAttributes)
    -> W.Chart.Internal.RenderData msg x y z
    -> SC.Svg msg
viewBars toBarsData_ toAxisData toYZData toPoints toYZConfig (W.Chart.Internal.RenderData d) =
    toYZData d
        |> Maybe.map
            (\yzData ->
                viewBarsWithOptions
                    { renderData = d
                    , barsData = toBarsData_ d
                    , toAxisData = toAxisData
                    , toPoints = toPoints
                    , yzData = yzData
                    , yzConfig = toYZConfig d
                    }
            )
        |> Maybe.withDefault (H.text "")


viewMixedBars :
    W.Chart.Internal.RenderData msg x y z
    -> SC.Svg msg
viewMixedBars (W.Chart.Internal.RenderData d) =
    Maybe.map2
        (\yData zData ->
            let
                barsData : BarsData y z
                barsData =
                    toBarsData d (Just yData) (Just zData)
            in
            S.g []
                [ viewBarsWithOptions
                    { renderData = d
                    , barsData = barsData
                    , toAxisData = .y
                    , toPoints = .ys
                    , yzData = yData
                    , yzConfig = d.attrs.yAxis
                    }
                , viewBarsWithOptions
                    { renderData = d
                    , barsData = barsData
                    , toAxisData = .z
                    , toPoints = .zs
                    , yzData = zData
                    , yzConfig = d.attrs.zAxis
                    }
                ]
        )
        d.y
        d.z
        |> Maybe.withDefault (H.text "")


viewBarsWithOptions :
    { renderData : W.Chart.Internal.RenderDataFull msg x y z
    , barsData : BarsData y z
    , toAxisData : BarsData y z -> AxisData a
    , toPoints : W.Chart.Internal.ChartPoint x y z -> List (W.Chart.Internal.DataPoint a)
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    }
    -> SC.Svg msg
viewBarsWithOptions props =
    props.renderData.points.byX
        |> Dict.values
        |> List.map
            (\point ->
                props.toPoints point
                    |> viewBinBars
                        { isHover = False
                        , renderData = props.renderData
                        , barsData = props.barsData
                        , axisData = props.toAxisData props.barsData
                        , xPoint = point.x
                        , yzData = props.yzData
                        , yzConfig = props.yzConfig
                        }
            )
        |> S.g []
