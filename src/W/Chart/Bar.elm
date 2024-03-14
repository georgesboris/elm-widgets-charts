module W.Chart.Bar exposing (yBars, zBars, yzBars)

{-|

@docs yBars, zBars, yzBars

-}

import Scale
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes


{-| -}
yBars : W.Chart.WidgetXY msg x y z a
yBars =
    W.Chart.Widget.fromY
        (viewBars
            (\ctx ->
                List.map
                    W.Chart.Internal.toDataPointsRender
                    ctx.y
            )
        )
        |> W.Chart.Widget.withHover
            (\ctx ->
                let
                    binScale : Scale.BandScale Int
                    binScale =
                        toBinScale ctx (binCount ctx.y)
                in
                \pointData ->
                    viewHover
                        binScale
                        pointData.x
                        pointData.y
            )


{-| -}
zBars : W.Chart.WidgetXYZ msg x y z a
zBars =
    W.Chart.Widget.fromZ
        (viewBars
            (\ctx ->
                List.map
                    W.Chart.Internal.toDataPointsRender
                    ctx.z
            )
        )
        |> W.Chart.Widget.withHover
            (\ctx ->
                let
                    binScale : Scale.BandScale Int
                    binScale =
                        toBinScale ctx (binCount ctx.z)
                in
                \pointData ->
                    viewHover binScale pointData.x pointData.z
            )


{-| -}
yzBars : W.Chart.WidgetXYZ msg x y z a
yzBars =
    W.Chart.Widget.fromYZ
        (viewBars
            (\ctx ->
                List.map W.Chart.Internal.toDataPointsRender ctx.y ++ List.map W.Chart.Internal.toDataPointsRender ctx.z
            )
        )
        |> W.Chart.Widget.withHover
            (\ctx ->
                let
                    binScale : Scale.BandScale Int
                    binScale =
                        toBinScale ctx (binCount ctx.y + binCount ctx.z)
                in
                \pointData ->
                    viewHover binScale pointData.x (pointData.y ++ pointData.z)
            )



-- Helpers


viewHover : Scale.BandScale Int -> W.Chart.Internal.RenderDatum -> List W.Chart.Internal.RenderDatum -> SC.Svg msg
viewHover binScale xPoint yzPoints =
    yzPoints
        |> List.indexedMap
            (\index yzPoint ->
                let
                    x : Float
                    x =
                        xPoint.valueStart + Scale.convert binScale index
                in
                S.rect
                    [ Svg.Attributes.fill yzPoint.color
                    , SAP.x x
                    , SAP.y yzPoint.valueStart
                    , SAP.width (Scale.bandwidth binScale)
                    , SAP.height (abs (yzPoint.valueStart - yzPoint.valueEnd))
                    , Svg.Attributes.stroke "white"
                    , SAP.strokeWidth 2
                    , W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 4.0
                        , color = yzPoint.color
                        }
                    ]
                    []
            )
        |> S.g []


viewBars : (W.Chart.Internal.ChartPointDict x y z -> List W.Chart.Internal.AxisDataPointsRender) -> W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewBars toPoints (W.Chart.Internal.RenderData { ctx }) =
    let
        data : List W.Chart.Internal.AxisDataPointsRender
        data =
            toPoints ctx.points

        binScale : Scale.BandScale Int
        binScale =
            toBinScale ctx (List.length data)
    in
    toPoints ctx.points
        |> List.map
            (\( axisDatum, points ) ->
                points
                    |> List.indexedMap
                        (\index ( xPoint, yzPoint ) ->
                            let
                                x : Float
                                x =
                                    xPoint.valueStart + Scale.convert binScale index
                            in
                            S.g
                                [ W.Chart.Internal.attrAnimationDelayX ctx x
                                , W.Chart.Internal.attrTransformOrigin x ctx.y.zero
                                , Svg.Attributes.class "ew-charts--animate-scale-z"
                                ]
                                [ S.rect
                                    [ Svg.Attributes.fill axisDatum.color
                                    , SAP.x x
                                    , SAP.y yzPoint.valueStart
                                    , SAP.width (Scale.bandwidth binScale)
                                    , SAP.height (abs (yzPoint.valueStart - yzPoint.valueEnd))
                                    ]
                                    []
                                ]
                        )
            )
        |> List.concat
        |> S.g []


binCount : W.Chart.Internal.RenderAxisYZ a -> Int
binCount axis =
    if axis.attrs.stackType == W.Chart.Internal.NoStack then
        List.length axis.data

    else
        1


toBinScale : W.Chart.RenderContext x y z -> Int -> Scale.BandScale Int
toBinScale ctx count =
    Scale.band
        { paddingInner = 0.2 -- TODO: Get from attrs
        , paddingOuter = 0.5 -- TODO: Get from attrs
        , align = 0.5
        }
        ( 0, Scale.bandwidth ctx.x.binScale )
        (List.range 0 (count - 1))
