module W.Chart.Bubble exposing
    ( fromY, fromZ
    , Attribute
    )

{-|

@docs fromY, fromZ


# Color

@docs Attribute

-}

import Attr
import Html as H
import Scale
import Svg.Attributes
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


{-| -}
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
fromY :
    List (Attribute msg point)
    ->
        { toRadius : W.Chart.Point x -> W.Chart.Point y -> Float
        , toColor : W.Chart.Point x -> W.Chart.Point y -> String
        }
    -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\_ props ->
            W.Chart.Widget.fromY
                (\ctx ->
                    view ctx ctx.points.y props
                )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            radiusScale : Scale.ContinuousScale Float
                            radiusScale =
                                toRadiusScale ctx.points.y props.toRadius
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
fromZ :
    List (Attribute msg point)
    ->
        { toRadius : W.Chart.Point x -> W.Chart.Point z -> Float
        , toColor : W.Chart.Point x -> W.Chart.Point z -> String
        }
    -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\_ props ->
            W.Chart.Widget.fromZ
                (\ctx ->
                    view ctx ctx.points.z props
                )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            radiusScale : Scale.ContinuousScale Float
                            radiusScale =
                                toRadiusScale ctx.points.z props.toRadius
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


view :
    W.Chart.Widget.Context x y z
    -> List (W.Chart.Internal.AxisDataPoints x a)
    ->
        { toRadius : W.Chart.Point x -> W.Chart.Point a -> Float
        , toColor : W.Chart.Point x -> W.Chart.Point a -> String
        }
    -> SC.Svg msg
view ctx axisData props =
    let
        radiusScale : Scale.ContinuousScale Float
        radiusScale =
            toRadiusScale axisData props.toRadius
    in
    axisData
        |> List.concatMap
            (\( _, points ) ->
                points
                    |> List.filterMap
                        (\( x, y ) ->
                            if y.render.isDefault then
                                Nothing

                            else
                                Just
                                    (S.g
                                        [ W.Chart.Internal.attrTransformOrigin x.render.valueScaled y.render.valueScaled
                                        , W.Chart.Internal.attrAnimationDelay ctx x.render.valueScaled y.render.valueScaled
                                        , SA.class [ "ew-charts--animate-scale" ]
                                        ]
                                        [ W.Svg.Circle.view
                                            [ Svg.Attributes.stroke y.render.color
                                            , Svg.Attributes.fill y.render.color
                                            , SA.fillOpacity (ST.Opacity 0.6)
                                            ]
                                            { x = x.render.valueScaled
                                            , y = y.render.valueScaled
                                            , radius = Scale.convert radiusScale (props.toRadius x y)
                                            }
                                        ]
                                    )
                        )
            )
        |> S.g []


toRadiusScale : List (W.Chart.Internal.AxisDataPoints x a) -> (W.Chart.Point x -> W.Chart.Point a -> Float) -> Scale.ContinuousScale Float
toRadiusScale axisData toRadius =
    axisData
        |> List.concatMap
            (\( _, points ) ->
                points
                    |> List.map (\( x, y ) -> toRadius x y)
            )
        |> W.Chart.Internal.bounds
        |> Scale.linear ( 4, 40 )


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
                    , y = yzPoint.valueScaled
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
