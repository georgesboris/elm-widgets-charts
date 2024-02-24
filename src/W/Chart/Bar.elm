module W.Chart.Bar exposing (yBars, zBars, yzBars)

{-|

@docs yBars, zBars, yzBars

-}

import Dict
import Html as H
import Scale
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes


{-| -}
yBars : W.Chart.Internal.Widget msg x y z { datasets | yData : () }
yBars =
    W.Chart.Widget.fromY (viewBars .y .ys (.attrs >> .yAxis))
        |> W.Chart.Widget.withHoverY
            (\d yData xPoint yPoints ->
                viewHover d xPoint (Just ( yData, yPoints )) Nothing
            )


{-| -}
zBars : W.Chart.Internal.Widget msg x y z { datasets | zData : () }
zBars =
    W.Chart.Widget.fromZ (viewBars .z .zs (.attrs >> .zAxis))
        |> W.Chart.Widget.withHoverZ
            (\d zData xPoint zPoints ->
                viewHover d xPoint Nothing (Just ( zData, zPoints ))
            )


{-| -}
yzBars : W.Chart.Internal.Widget msg x y z { datasets | yData : (), zData : () }
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


viewHover :
    W.Chart.Internal.RenderDataFull msg x y z
    -> W.Chart.Internal.DataPoint x
    -> Maybe ( W.Chart.Internal.RenderDataYZ x y, List (W.Chart.Internal.DataPoint y) )
    -> Maybe ( W.Chart.Internal.RenderDataYZ x z, List (W.Chart.Internal.DataPoint z) )
    -> SC.Svg msg
viewHover renderData xPoint maybeYPoints maybeZPoints =
    let
        yBinsCount : Int
        yBinsCount =
            maybeYPoints
                |> Maybe.map
                    (\( yData, _ ) ->
                        if renderData.attrs.yAxis.stackType == W.Chart.Internal.NoStack then
                            List.length yData.data

                        else
                            1
                    )
                |> Maybe.withDefault 0

        zBinsCount : Int
        zBinsCount =
            maybeZPoints
                |> Maybe.map
                    (\( zData, _ ) ->
                        if renderData.attrs.zAxis.stackType == W.Chart.Internal.NoStack then
                            List.length zData.data

                        else
                            1
                    )
                |> Maybe.withDefault 0

        binsCount : Int
        binsCount =
            yBinsCount
                + zBinsCount

        binScale : Scale.BandScale Int
        binScale =
            Scale.band
                { paddingInner = renderData.attrs.binPaddingInner
                , paddingOuter = renderData.attrs.binPaddingOuter
                , align = 0.5
                }
                ( 0, Scale.bandwidth renderData.x.bandScale )
                (List.range 0 (binsCount - 1))

        toX : W.Chart.Internal.StackType -> Int -> Int -> Float
        toX stackType binsOffset binIndex =
            if stackType == W.Chart.Internal.NoStack then
                xPoint.valueStart + Scale.convert binScale (binsOffset + binIndex)

            else
                xPoint.valueStart

        width : Float
        width =
            Scale.bandwidth binScale
    in
    S.g []
        [ maybeYPoints
            |> Maybe.map
                (\( yData, yPoints ) ->
                    viewBinBars
                        { isHover = True
                        , renderData = renderData
                        , yzPoints = yPoints
                        , yzData = yData
                        , yzConfig = renderData.attrs.yAxis
                        , binsOffset = 0
                        , binWidth = width
                        , toX = toX
                        }
                )
            |> Maybe.withDefault (H.text "")
        , maybeZPoints
            |> Maybe.map
                (\( zData, zPoints ) ->
                    viewBinBars
                        { isHover = True
                        , renderData = renderData
                        , yzPoints = zPoints
                        , yzData = zData
                        , yzConfig = renderData.attrs.zAxis
                        , binsOffset = yBinsCount
                        , binWidth = width
                        , toX = toX
                        }
                )
            |> Maybe.withDefault (H.text "")
        ]


viewBinBars :
    { isHover : Bool
    , renderData : W.Chart.Internal.RenderDataFull msg x y z
    , yzPoints : List (W.Chart.Internal.DataPoint a)
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    , binsOffset : Int
    , binWidth : Float
    , toX : W.Chart.Internal.StackType -> Int -> Int -> Float
    }
    -> SC.Svg msg
viewBinBars props =
    let
        yzZero : Float
        yzZero =
            Scale.convert props.yzData.scale 0.0
    in
    props.yzPoints
        |> List.indexedMap
            (\binIndex yzPoint ->
                let
                    color : String
                    color =
                        props.yzData.toColor yzPoint.datum

                    y : Float
                    y =
                        if props.yzConfig.stackType == W.Chart.Internal.NoStack && yzPoint.valueStart > yzZero then
                            yzZero

                        else
                            yzPoint.valueStart

                    height : Float
                    height =
                        abs (yzPoint.valueStart - yzPoint.valueEnd)

                    x : Float
                    x =
                        props.toX props.yzConfig.stackType props.binsOffset binIndex

                    bar : SC.Svg msg
                    bar =
                        S.rect
                            [ Svg.Attributes.fill color
                            , SAP.x (props.toX props.yzConfig.stackType props.binsOffset binIndex)
                            , SAP.y y
                            , SAP.width props.binWidth
                            , SAP.height height
                            , W.Svg.Attributes.cond props.isHover
                                (W.Svg.Attributes.dropShadow
                                    { xOffset = 0
                                    , yOffset = 0
                                    , radius = 2.0
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
                        , W.Chart.Internal.attrTransformOrigin x yzZero
                        , Svg.Attributes.class "ew-charts--animate-scale-z"
                        ]
                        [ bar ]
            )
        |> S.g []


viewBars :
    (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    -> (W.Chart.Internal.ChartPoint x y z -> List (W.Chart.Internal.DataPoint a))
    -> (W.Chart.Internal.RenderDataFull msg x y z -> W.Chart.Internal.AxisAttributes)
    -> W.Chart.Internal.RenderData msg x y z constraints
    -> SC.Svg msg
viewBars toYZData toPoints toYZConfig (W.Chart.Internal.RenderData d) =
    toYZData d
        |> Maybe.map
            (\yzData ->
                viewBarsWithOptions
                    { renderData = d
                    , toPoints = toPoints
                    , yzData = yzData
                    , yzConfig = toYZConfig d
                    , binsCount = List.length yzData.data
                    , binsOffset = 0
                    }
            )
        |> Maybe.withDefault (H.text "")


viewMixedBars :
    W.Chart.Internal.RenderData msg x y z constraints
    -> SC.Svg msg
viewMixedBars (W.Chart.Internal.RenderData d) =
    Maybe.map2
        (\yData zData ->
            let
                yBinsCount : Int
                yBinsCount =
                    if d.attrs.yAxis.stackType == W.Chart.Internal.NoStack then
                        List.length yData.data

                    else
                        1

                zBinsCount : Int
                zBinsCount =
                    if d.attrs.zAxis.stackType == W.Chart.Internal.NoStack then
                        List.length zData.data

                    else
                        1

                binsCount : Int
                binsCount =
                    yBinsCount + zBinsCount
            in
            S.g []
                [ viewBarsWithOptions
                    { renderData = d
                    , toPoints = .ys
                    , yzData = yData
                    , yzConfig = d.attrs.yAxis
                    , binsCount = binsCount
                    , binsOffset = 0
                    }
                , viewBarsWithOptions
                    { renderData = d
                    , toPoints = .zs
                    , yzData = zData
                    , yzConfig = d.attrs.zAxis
                    , binsCount = binsCount
                    , binsOffset = yBinsCount
                    }
                ]
        )
        d.y
        d.z
        |> Maybe.withDefault (H.text "")


viewBarsWithOptions :
    { renderData : W.Chart.Internal.RenderDataFull msg x y z
    , toPoints : W.Chart.Internal.ChartPoint x y z -> List (W.Chart.Internal.DataPoint a)
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    , binsCount : Int
    , binsOffset : Int
    }
    -> SC.Svg msg
viewBarsWithOptions props =
    let
        binScale : Scale.BandScale Int
        binScale =
            Scale.band
                { paddingInner = props.renderData.attrs.binPaddingInner
                , paddingOuter = props.renderData.attrs.binPaddingOuter
                , align = 0.5
                }
                ( 0, Scale.bandwidth props.renderData.x.bandScale )
                (List.range 0 (props.binsCount - 1))

        toX : x -> W.Chart.Internal.StackType -> Int -> Int -> Float
        toX xDatum stackType binsOffset binIndex =
            if stackType == W.Chart.Internal.NoStack then
                Scale.convert props.renderData.x.bandScale xDatum
                    + Scale.convert binScale (binsOffset + binIndex)

            else
                Scale.convert
                    props.renderData.x.bandScale
                    xDatum

        width : Float
        width =
            Scale.bandwidth binScale
    in
    props.renderData.points.byX
        |> Dict.values
        |> List.map
            (\point ->
                viewBinBars
                    { isHover = False
                    , renderData = props.renderData
                    , yzPoints = props.toPoints point
                    , yzData = props.yzData
                    , yzConfig = props.yzConfig
                    , binsOffset = props.binsOffset
                    , binWidth = width
                    , toX = toX point.x.datum
                    }
            )
        |> S.g []
