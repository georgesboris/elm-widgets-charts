module W.Chart.Bar exposing
    ( fromY, fromZ, fromYZ
    , margins, Attribute
    )

{-|

@docs fromY, fromZ, fromYZ

@docs margins, Attribute

-}

import Attr
import Scale
import Svg
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes


{-| -}
fromY : List Attribute -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY
                (\ctx ->
                    viewBars
                        ctx
                        ctx.y
                        (toBinScale attrs ctx (binCount ctx.y))
                        (toIndexed ctx.y 0 ctx.points.y)
                )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx (binCount ctx.y)
                        in
                        \pointData ->
                            viewHover
                                ctx.y
                                binScale
                                pointData.x
                                (toIndexed ctx.y 0 pointData.y)
                    )
        )


{-| -}
fromZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ
                (\ctx ->
                    viewBars
                        ctx
                        ctx.z
                        (toBinScale attrs ctx (binCount ctx.z))
                        (toIndexed ctx.z 0 ctx.points.z)
                )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx (binCount ctx.z)
                        in
                        \pointData ->
                            viewHover ctx.z binScale pointData.x (toIndexed ctx.z 0 pointData.z)
                    )
        )


{-| -}
fromYZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
fromYZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromYZ
                (\ctx ->
                    let
                        yCount : Int
                        yCount =
                            binCount ctx.y

                        zCount : Int
                        zCount =
                            binCount ctx.z

                        binScale : Scale.BandScale Int
                        binScale =
                            toBinScale attrs ctx (yCount + zCount)
                    in
                    S.g []
                        [ viewBars ctx ctx.y binScale (toIndexed ctx.y 0 ctx.points.y)
                        , viewBars ctx ctx.z binScale (toIndexed ctx.z yCount ctx.points.z)
                        ]
                )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            yCount : Int
                            yCount =
                                binCount ctx.y

                            zCount : Int
                            zCount =
                                binCount ctx.z

                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx (yCount + zCount)
                        in
                        \pointData ->
                            let
                                yItems : List ( Int, W.Chart.RenderDatum )
                                yItems =
                                    toIndexed ctx.y 0 pointData.y

                                zItems : List ( Int, W.Chart.RenderDatum )
                                zItems =
                                    toIndexed ctx.z yCount pointData.z
                            in
                            S.g []
                                [ viewHover ctx.y binScale pointData.x yItems
                                , viewHover ctx.z binScale pointData.x zItems
                                ]
                    )
        )



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { outerMargin : Float
    , innerMargin : Float
    }


defaultAttrs : Attributes
defaultAttrs =
    { innerMargin = 0.2
    , outerMargin = 0.5
    }


{-| -}
margins : Float -> Float -> Attribute
margins innerMargin outerMargin =
    Attr.attr (\attr -> { attr | innerMargin = innerMargin, outerMargin = outerMargin })



-- Helpers


viewHover : W.Chart.Internal.RenderAxisYZ a -> Scale.BandScale Int -> W.Chart.Internal.RenderDatum -> List ( Int, W.Chart.Internal.RenderDatum ) -> SC.Svg msg
viewHover axis binScale xPoint yzPoints =
    yzPoints
        |> List.map
            (\( index, yzPoint ) ->
                let
                    x : Float
                    x =
                        xPoint.valueStart + Scale.convert binScale index
                in
                viewBar
                    [ Svg.Attributes.stroke "white"
                    , SAP.strokeWidth 2
                    , W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 4.0
                        , color = yzPoint.color
                        }
                    ]
                    { color = yzPoint.color
                    , x = x
                    , y = yzPoint.valueStart
                    , width = Scale.bandwidth binScale
                    , height = abs (yzPoint.valueStart - yzPoint.valueEnd)
                    , axis = axis
                    }
            )
        |> S.g []


viewBars : W.Chart.Widget.Context x y z -> W.Chart.Internal.RenderAxisYZ a -> Scale.BandScale Int -> List ( Int, W.Chart.Internal.AxisDataPoints x a ) -> SC.Svg msg
viewBars ctx axis binScale indexedAxes =
    indexedAxes
        |> List.concatMap
            (\( index, ( axisDatum, points ) ) ->
                points
                    |> List.map
                        (\( xPoint, yzPoint ) ->
                            let
                                x : Float
                                x =
                                    xPoint.render.valueStart + Scale.convert binScale index
                            in
                            S.g
                                [ W.Chart.Internal.attrAnimationDelayX ctx x
                                , W.Chart.Internal.attrTransformOrigin x axis.zero
                                , Svg.Attributes.class "ew-charts--animate-scale-z"
                                ]
                                [ viewBar []
                                    { color = axisDatum.color
                                    , x = x
                                    , y = yzPoint.render.valueStart
                                    , width = Scale.bandwidth binScale
                                    , height = abs (yzPoint.render.valueStart - yzPoint.render.valueEnd)
                                    , axis = axis
                                    }
                                ]
                        )
            )
        |> S.g []


viewBar :
    List (Svg.Attribute msg)
    ->
        { color : String
        , x : Float
        , y : Float
        , width : Float
        , height : Float
        , axis : W.Chart.Internal.RenderAxisYZ a
        }
    -> SC.Svg msg
viewBar attrs props =
    S.rect
        ([ Svg.Attributes.fill props.color
         , SAP.x props.x
         , SAP.width props.width
         , SAP.height props.height
         , if props.axis.isStacked || props.y < props.axis.zero then
            SAP.y props.y

           else
            SAP.y props.axis.zero
         ]
            ++ attrs
        )
        []


binCount : W.Chart.Internal.RenderAxisYZ a -> Int
binCount axis =
    if axis.isStacked then
        1

    else
        List.length axis.data


toIndexed : W.Chart.Internal.RenderAxisYZ a -> Int -> List item -> List ( Int, item )
toIndexed axis offset points =
    if axis.isStacked then
        List.map (Tuple.pair offset) points

    else
        List.indexedMap (\index a -> ( index + offset, a )) points


toBinScale : Attributes -> W.Chart.Widget.Context x y z -> Int -> Scale.BandScale Int
toBinScale attrs ctx count =
    Scale.band
        { paddingInner = attrs.innerMargin
        , paddingOuter = attrs.outerMargin
        , align = 0.5
        }
        ( 0, Scale.bandwidth ctx.x.binScale )
        (List.range 0 (count - 1))
