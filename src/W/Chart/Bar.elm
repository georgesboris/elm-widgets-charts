module W.Chart.Bar exposing (yBars, yzBars, zBars)

import Html as H
import Scale
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart.ChartElement
import W.Chart.Internal
import W.Svg.Attributes
import W.Svg.Circle


yBars : W.Chart.Internal.ChartElement msg x y z { datasets | yData : () }
yBars =
    W.Chart.ChartElement.fromY (viewBars .y (.attrs >> .yAxis))
        |> W.Chart.ChartElement.withHoverY (\_ -> viewHover)


zBars : W.Chart.Internal.ChartElement msg x y z { datasets | zData : () }
zBars =
    W.Chart.ChartElement.fromZ (viewBars .z (.attrs >> .zAxis))
        |> W.Chart.ChartElement.withHoverZ (\_ -> viewHover)


yzBars : W.Chart.Internal.ChartElement msg x y z { datasets | yData : (), zData : () }
yzBars =
    W.Chart.ChartElement.fromYZ viewMixedBars
        |> W.Chart.ChartElement.withHoverYZ
            (\_ yData zData point ->
                [ List.map (viewHover yData point.x) point.ys
                , List.map (viewHover zData point.x) point.zs
                ]
                    |> List.concat
                    |> S.g []
            )



-- Helpers


viewHover :
    W.Chart.Internal.RenderDataYZ x a
    -> W.Chart.Internal.DataPoint x
    -> W.Chart.Internal.DataPoint a
    -> SC.Svg msg
viewHover yzData xPoint yzPoint =
    W.Svg.Circle.view
        [ Svg.Attributes.fill Theme.baseBackground
        , Svg.Attributes.stroke (yzData.toColor yzPoint.datum)
        , W.Svg.Attributes.dropShadow
            { xOffset = 0
            , yOffset = 0
            , radius = 4.0
            , color = yzData.toColor yzPoint.datum
            }
        ]
        { x = xPoint.valueScaled
        , y = yzPoint.valueScaled
        , radius = 4.0
        }


viewBars :
    (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    -> (W.Chart.Internal.RenderDataFull msg x y z -> W.Chart.Internal.AxisAttributes)
    -> W.Chart.Internal.RenderData msg x y z constraints
    -> SC.Svg msg
viewBars toYZData toYZConfig (W.Chart.Internal.RenderData d) =
    toYZData d
        |> Maybe.map
            (\yzData ->
                viewBarsWithOptions
                    { renderData = d
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
                    , yzData = yData
                    , yzConfig = d.attrs.yAxis
                    , binsCount = binsCount
                    , binsOffset = 0
                    }
                , viewBarsWithOptions
                    { renderData = d
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


viewMixedBarsBin :
    W.Chart.Internal.RenderDataFull msg x y z
    -> W.Chart.Internal.RenderDataYZ x y
    -> W.Chart.Internal.RenderDataYZ x z
    -> W.Chart.Internal.ChartPoint x y z
    -> SC.Svg msg
viewMixedBarsBin d yData zData point =
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

        binScale : Scale.BandScale Int
        binScale =
            Scale.band
                { paddingInner = d.attrs.binPaddingInner
                , paddingOuter = d.attrs.binPaddingOuter
                , align = 0.5
                }
                ( 0, Scale.bandwidth d.x.bandScale )
                (List.range 0 (binsCount - 1))
    in
    S.g []
        [ viewBarsWithOptions
            { renderData = d
            , yzData = yData
            , yzConfig = d.attrs.yAxis
            , binsCount = binsCount
            , binsOffset = 0
            }
        , viewBarsWithOptions
            { renderData = d
            , yzData = zData
            , yzConfig = d.attrs.zAxis
            , binsCount = binsCount
            , binsOffset = yBinsCount
            }
        ]


viewBinBars :
    { renderData : W.Chart.Internal.RenderDataFull msg x y z
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , yzConfig : W.Chart.Internal.AxisAttributes
    , xPoint : W.Chart.Internal.DataPoint x
    , yzPoints : List (W.Chart.Internal.DataPoint a)
    , binsCount : Int
    , binsOffset : Int
    , binScale : Scale.BandScale Int
    , binWidth : Float
    }
    -> List (SC.Svg msg)
viewBinBars props =
    let
        yzZero : Float
        yzZero =
            Scale.convert props.yzData.scale 0.0

        binX : Float
        binX =
            Scale.convert props.renderData.x.bandScale props.xPoint.datum

        toX : Int -> Float
        toX binIndex =
            if props.yzConfig.stackType == W.Chart.Internal.NoStack then
                binX + Scale.convert props.binScale (props.binsOffset + binIndex)

            else
                binX
    in
    props.yzPoints
        |> List.indexedMap
            (\index yzPoint ->
                let
                    color : String
                    color =
                        props.yzData.toColor yzPoint.datum
                in
                List.map2
                    (\xDatum ( low, high ) ->
                        let
                            positive : Bool
                            positive =
                                high >= 0.0

                            y : Float
                            y =
                                if positive then
                                    Scale.convert props.yzData.scale high

                                else
                                    yzZero

                            height : Float
                            height =
                                if positive then
                                    abs
                                        (Scale.convert props.yzData.scale low - Scale.convert props.yzData.scale high)

                                else
                                    Scale.convert props.yzData.scale high - yzZero
                        in
                        S.rect
                            [ Svg.Attributes.fill color
                            , SAP.x (toX index)
                            , SAP.y y
                            , SAP.width props.binWidth
                            , SAP.height height
                            ]
                            []
                    )
            )
        |> Debug.todo ""


viewBarsWithOptions :
    { renderData : W.Chart.Internal.RenderDataFull msg x y z
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

        yzZero : Float
        yzZero =
            Scale.convert props.yzData.scale 0.0

        toX : x -> Int -> Float
        toX xDatum binIndex =
            if props.yzConfig.stackType == W.Chart.Internal.NoStack then
                Scale.convert props.renderData.x.bandScale xDatum
                    + Scale.convert binScale (props.binsOffset + binIndex)

            else
                Scale.convert
                    props.renderData.x.bandScale
                    xDatum

        width : Float
        width =
            Scale.bandwidth binScale
    in
    props.yzData.bandData
        |> List.indexedMap
            (\binIndex ( datum, values ) ->
                let
                    color : String
                    color =
                        props.yzData.toColor datum
                in
                List.map2
                    (\xDatum ( low, high ) ->
                        let
                            positive : Bool
                            positive =
                                high >= 0.0

                            y : Float
                            y =
                                if positive then
                                    Scale.convert props.yzData.scale high

                                else
                                    yzZero

                            height : Float
                            height =
                                if positive then
                                    abs
                                        (Scale.convert props.yzData.scale low - Scale.convert props.yzData.scale high)

                                else
                                    Scale.convert props.yzData.scale high - yzZero
                        in
                        S.rect
                            [ Svg.Attributes.fill color
                            , SAP.x (toX xDatum binIndex)
                            , SAP.y y
                            , SAP.width width
                            , SAP.height height
                            ]
                            []
                    )
                    props.renderData.x.data
                    values
            )
        |> List.concat
        |> W.Chart.Internal.viewTranslateChart props.renderData.spacings
