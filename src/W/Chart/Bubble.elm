module W.Chart.Bubble exposing
    ( viewY, viewZ
    , Attribute
    )

{-|

@docs viewY, viewZ


# Color

@docs Attribute
@docs colorFromPercentile, colorFromPercentiles, colorFromRadius, colorFromRadiusPercentile, colorFromValue
@docs colorCustomY, colorCustomZ

-}

import Attr
import Dict
import Html as H
import Scale
import Svg
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes
import W.Svg.Circle



-- Attributes


type alias Attribute msg point =
    Attr.Attr (Attributes msg point)


type alias Attributes msg point =
    { point : Maybe point
    , htmlAttrs : List (H.Attribute msg)
    }


defaultAttrs : Attributes msg point
defaultAttrs =
    { point = Nothing
    , htmlAttrs = []
    }



-- View


{-| -}
viewY :
    List (Attribute msg point)
    ->
        { toRadius : W.Chart.Point x -> W.Chart.Point y -> Float
        , toColor : W.Chart.Point x -> W.Chart.Point y -> String
        }
    -> W.Chart.WidgetXY msg x y z a
viewY =
    Attr.withAttrs defaultAttrs
        (\attrs props ->
            W.Chart.Widget.fromY view
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            radiusScale : Scale.ContinuousScale Float
                            radiusScale =
                                ctx.points.byX
                                    |> Dict.values
                                    |> List.concatMap (\d -> List.map (.render >> .value) d.ys)
                                    |> W.Chart.Internal.bounds
                                    |> Scale.linear ( 4, 40 )
                        in
                        \pointData ->
                            viewHover
                                pointData.x
                                (List.map2
                                    (\yRender yPoint ->
                                        ( Scale.convert radiusScale (props.toRadius pointData.point.x yPoint)
                                        , props.toColor pointData.point.x yPoint
                                        , yRender
                                        )
                                    )
                                    pointData.y
                                    pointData.point.y
                                )
                    )
        )


{-| -}
viewZ :
    List (Attribute msg point)
    ->
        { toRadius : W.Chart.Point x -> W.Chart.Point z -> Float
        , toColor : W.Chart.Point x -> W.Chart.Point z -> String
        }
    -> W.Chart.WidgetXYZ msg x y z a
viewZ =
    Attr.withAttrs defaultAttrs
        (\attrs props ->
            W.Chart.Widget.fromZ view
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            radiusScale : Scale.ContinuousScale Float
                            radiusScale =
                                ctx.points.byX
                                    |> Dict.values
                                    |> List.concatMap (\d -> List.map (.render >> .value) d.zs)
                                    |> W.Chart.Internal.bounds
                                    |> Scale.linear ( 4, 40 )
                        in
                        \pointData ->
                            viewHover
                                pointData.x
                                (List.map2
                                    (\zRender zPoint ->
                                        ( Scale.convert radiusScale (props.toRadius pointData.point.x zPoint)
                                        , props.toColor pointData.point.x zPoint
                                        , zRender
                                        )
                                    )
                                    pointData.z
                                    pointData.point.z
                                )
                    )
        )


view _ =
    H.text ""


{-| -}
viewHover : W.Chart.RenderDatum -> List ( Float, String, W.Chart.RenderDatum ) -> SC.Svg msg
viewHover xPoint yzPoints =
    yzPoints
        |> List.concatMap
            (\( radius, color, yzPoint ) ->
                [ W.Svg.Circle.view
                    [ Svg.Attributes.stroke color
                    , Svg.Attributes.fill color
                    , SA.fillOpacity (ST.Opacity 0.6)
                    , W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 8.0
                        , color = color
                        }
                    ]
                    { x = xPoint.valueScaled
                    , y = yzPoint.valueStart
                    , radius = radius
                    }
                , W.Svg.Circle.view
                    [ Svg.Attributes.fill "transparent"
                    , Svg.Attributes.stroke "white"
                    ]
                    { x = xPoint.valueScaled
                    , y = yzPoint.valueStart
                    , radius = radius + 2
                    }
                ]
            )
        |> S.g []



-- View : Helpers
-- viewOld :
--     W.Chart.Internal.RenderData msg x y z
--     -> Attributes msg point
--     ->
--         { toRadius : W.Chart.RenderContext x y z -> W.Chart.Point x -> W.Chart.Point y -> Float
--         , toColor : W.Chart.RenderContext x y z -> W.Chart.Point x -> W.Chart.Point y -> String
--         }
--     -> SC.Svg msg
-- viewOld attrs props toYZ toYZPoints withHover props attrs =
--     W.Chart.Widget.fromX
--         (\(W.Chart.Internal.RenderData d) ->
--             toYZ d
--                 |> Maybe.map
--                     (\yzData ->
--                         let
--                             bubbleData : BubbleData x a
--                             bubbleData =
--                                 toBubbleData
--                                     { data = d
--                                     , yzData = yzData
--                                     , toYZPoints = toYZPoints
--                                     , toRadius = props.toRadius
--                                     }
--                         in
--                         bubbleData.points
--                             |> List.map
--                                 (\point ->
--                                     let
--                                         color : String
--                                         color =
--                                             attrs.toColor
--                                                 { x = point.x
--                                                 , yz = point.yz
--                                                 , yzDomain = Scale.domain yzData.scale
--                                                 , yzColor = yzData.toColor point.yz.datum
--                                                 , radius = point.radius
--                                                 , radiusDomain = bubbleData.radiusDomain
--                                                 }
--                                     in
--                                     S.g
--                                         [ W.Chart.Internal.attrTransformOrigin point.x.valueScaled point.yz.valueScaled
--                                         , W.Chart.Internal.attrAnimationDelay d.spacings point.x.valueScaled point.yz.valueScaled
--                                         , SA.class [ "ew-charts--animate-scale" ]
--                                         ]
--                                         [ W.Svg.Circle.view
--                                             [ Svg.Attributes.fill Theme.baseBackground
--                                             , SA.fillOpacity (ST.Opacity 0.6)
--                                             , Svg.Attributes.stroke color
--                                             , Svg.Attributes.fill color
--                                             ]
--                                             { x = point.x.valueScaled
--                                             , y = point.yz.valueStart
--                                             , radius = Scale.convert bubbleData.radiusScale point.radius
--                                             }
--                                         ]
--                                 )
--                             |> S.g []
--                     )
--                 |> Maybe.withDefault (H.text "")
--         )
-- viewBubble :
--     List (Svg.Attribute msg)
--     -> W.Chart.Internal.RenderDataYZ x a
--     -> AttributesYZ msg x a
--     -> BubbleData x a
--     ->
--         { x : W.Chart.Internal.DataPoint x
--         , yz : W.Chart.Internal.DataPoint a
--         , radius : Float
--         }
--     -> Svg.Svg msg
-- viewBubble svgAttrs yzData attrs bubbleData point =
--     let
--         color : String
--         color =
--             attrs.toColor
--                 { x = point.x
--                 , yz = point.yz
--                 , yzDomain = Scale.domain yzData.scale
--                 , yzColor = yzData.toColor point.yz.datum
--                 , radius = point.radius
--                 , radiusDomain = bubbleData.radiusDomain
--                 }
--     in
--     W.Svg.Circle.view
--         (svgAttrs
--             ++ [ Svg.Attributes.fill Theme.baseBackground
--                , SA.fillOpacity (ST.Opacity 0.6)
--                , Svg.Attributes.stroke color
--                , Svg.Attributes.fill color
--                ]
--         )
--         { x = point.x.valueScaled
--         , y = point.yz.valueStart
--         , radius = Scale.convert bubbleData.radiusScale point.radius
--         }
-- viewBubble2 :
--     List (Svg.Attribute msg)
--     -> W.Chart.RenderContext x y z
--     -> W.Chart.Internal.ChartPointData point
--     -- -> W.Chart.Internal.RenderDataYZ x a
--     -- -> AttributesYZ msg x a
--     -- -> BubbleData x a
--     ->
--         { x : W.Chart.Internal.DataPoint x
--         , yz : W.Chart.Internal.DataPoint a
--         , radius : Float
--         }
--     -> Svg.Svg msg
-- viewBubble2 svgAttrs ctx pointData =
--     let
--         color : String
--         color =
--             attrs.toColor
--                 { x = point.x
--                 , yz = point.yz
--                 , yzDomain = Scale.domain yzData.scale
--                 , yzColor = yzData.toColor point.yz.datum
--                 , radius = point.radius
--                 , radiusDomain = bubbleData.radiusDomain
--                 }
--     in
--     W.Svg.Circle.view
--         (svgAttrs
--             ++ [ Svg.Attributes.fill Theme.baseBackground
--                , SA.fillOpacity (ST.Opacity 0.6)
--                , Svg.Attributes.stroke color
--                , Svg.Attributes.fill color
--                ]
--         )
--         { x = pointData.pos x.valueScaled
--         , y = pointData.pos point.yz.valueStart
--         , radius = Scale.convert bubbleData.radiusScale point.radius
--         }
-- type alias PointData x a =
--     { x : W.Chart.Internal.DataPoint x
--     , yz : W.Chart.Internal.DataPoint a
--     , yzDomain : ( Float, Float )
--     , yzColor : String
--     , radiusDomain : ( Float, Float )
--     , radius : Float
--     }
-- type alias BubbleData x a =
--     { points :
--         List
--             { x : W.Chart.Internal.DataPoint x
--             , yz : W.Chart.Internal.DataPoint a
--             , radius : Float
--             }
--     , radiusScale : Scale.ContinuousScale Float
--     , radiusDomain : ( Float, Float )
--     }
-- applyAttrs : (AttributesYZ msg x a -> Attributes msg point) -> List (Attribute msg point) -> Attributes msg point
-- applyAttrs toDefaultAttrs attrs =
--     List.foldl (\(Attribute fn) a -> fn a) (toDefaultAttrs defaultAttrs) attrs
-- defaultAttrs : AttributesYZ msg x a
-- defaultAttrs =
--     { toColor = .yzColor
--     , htmlAttrs = []
--     }
-- toAttrsY : Attributes msg point -> Maybe (AttributesYZ msg x y)
-- toAttrsY attrs =
--     case attrs of
--         AttributesY a ->
--             Just a
--         AttributesZ _ ->
--             Nothing
-- toAttrsZ : Attributes msg point -> Maybe (AttributesYZ msg x z)
-- toAttrsZ attrs =
--     case attrs of
--         AttributesY _ ->
--             Nothing
--         AttributesZ a ->
--             Just a
-- toBubbleData :
--     { data : W.Chart.Internal.RenderDataFull msg point
--     , yzData : W.Chart.Internal.RenderDataYZ x a
--     , toYZPoints : W.Chart.Internal.ChartPoint x y z -> List (W.Chart.Internal.DataPoint a)
--     , toRadius : x -> ( a, Float ) -> Float
--     }
--     -> BubbleData x a
-- toBubbleData props =
--     let
--         minBubbleSize : Float
--         minBubbleSize =
--             4
--         maxBubbleSize : Float
--         maxBubbleSize =
--             props.data.spacings.chart.height * 0.125
--         points :
--             List
--                 { x : W.Chart.Internal.DataPoint x
--                 , yz : W.Chart.Internal.DataPoint a
--                 , radius : Float
--                 }
--         points =
--             props.data.points.byX
--                 |> Dict.values
--                 |> List.map
--                     (\xData ->
--                         props.toYZPoints xData
--                             |> List.filterMap
--                                 (\yzData ->
--                                     if yzData.missing then
--                                         Nothing
--                                     else
--                                         Just
--                                             { x = xData.x
--                                             , yz = yzData
--                                             , radius = props.toRadius xData.x.datum ( yzData.datum, yzData.value )
--                                             }
--                                 )
--                     )
--                 |> List.concat
--         radiusDomain : ( Float, Float )
--         radiusDomain =
--             points
--                 |> W.Chart.Internal.boundsAt .radius
--                 |> Maybe.map (\b -> ( b.min, b.max ))
--                 |> Maybe.withDefault ( 0.0, 0.0 )
--     in
--     { points = points
--     , radiusDomain = radiusDomain
--     , radiusScale =
--         Scale.linear
--             ( minBubbleSize, maxBubbleSize )
--             radiusDomain
--     }
-- Attributes : Setters
-- {-| -}
-- colorFromValue : (Float -> String) -> Attribute msg point
-- colorFromValue v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = \data -> v data.yz.value }
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = \data -> v data.yz.value }
--         )
-- {-| -}
-- colorFromRadius : (Float -> String) -> Attribute msg point
-- colorFromRadius v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = \data -> v data.radius }
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = \data -> v data.radius }
--         )
-- {-| -}
-- colorFromPercentile : (Float -> String) -> Attribute msg point
-- colorFromPercentile v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) }
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) }
--         )
-- {-| -}
-- colorFromRadiusPercentile : (Float -> String) -> Attribute msg point
-- colorFromRadiusPercentile v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = \data -> v (toValueInPercentil data.radiusDomain data.radius) }
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.radiusDomain data.radius) }
--         )
-- {-| -}
-- colorFromPercentiles : (Float -> Float -> String) -> Attribute msg point
-- colorFromPercentiles v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) (toValueInPercentil data.radiusDomain data.radius) }
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) (toValueInPercentil data.radiusDomain data.radius) }
--         )
-- {-| -}
-- colorCustomY :
--     ({ x : W.Chart.Internal.DataPoint x
--      , yz : W.Chart.Internal.DataPoint y
--      , yzDomain : ( Float, Float )
--      , yzColor : String
--      , radiusDomain : ( Float, Float )
--      , radius : Float
--      }
--      -> String
--     )
--     -> Attribute msg point
-- colorCustomY v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY { attr | toColor = v }
--                 AttributesZ attr ->
--                     AttributesZ attr
--         )
-- {-| -}
-- colorCustomZ :
--     ({ x : W.Chart.Internal.DataPoint x
--      , yz : W.Chart.Internal.DataPoint z
--      , yzDomain : ( Float, Float )
--      , yzColor : String
--      , radiusDomain : ( Float, Float )
--      , radius : Float
--      }
--      -> String
--     )
--     -> Attribute msg point
-- colorCustomZ v =
--     Attribute
--         (\a ->
--             case a of
--                 AttributesY attr ->
--                     AttributesY attr
--                 AttributesZ attr ->
--                     AttributesZ { attr | toColor = v }
--         )
-- -- Helpers
-- toValueInPercentil : ( Float, Float ) -> Float -> Float
-- toValueInPercentil ( min, max ) value =
--     (value - min) / (max - min)
