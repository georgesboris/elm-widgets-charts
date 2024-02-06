module W.Chart.Bubble exposing
    ( Attribute
    , colorCustomY
    , colorCustomZ
    , colorFromPercentile
    , colorFromPercentiles
    , colorFromRadius
    , colorFromRadiusPercentile
    , colorFromValue
    , viewY
    , viewZ
    )

import Html as H
import Scale
import Svg
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Types as ST
import W.Chart
import W.Chart.ChartElement
import W.Chart.Internal
import W.Svg.Attributes
import W.Svg.Circle



-- View


viewY :
    List (Attribute msg x y z)
    -> { toRadius : x -> ( y, Float ) -> Float }
    -> W.Chart.ChartElement msg x y z { datasets | yData : () }
viewY attrs_ props =
    attrs_
        |> applyAttrs AttributesY
        |> toAttrsY
        |> Maybe.map (view .y W.Chart.ChartElement.withHoverY props)
        |> Maybe.withDefault W.Chart.ChartElement.empty


viewZ :
    List (Attribute msg x y z)
    -> { toRadius : x -> ( z, Float ) -> Float }
    -> W.Chart.ChartElement msg x y z { datasets | zData : () }
viewZ attrs_ props =
    attrs_
        |> applyAttrs AttributesZ
        |> toAttrsZ
        |> Maybe.map (view .z W.Chart.ChartElement.withHoverZ props)
        |> Maybe.withDefault W.Chart.ChartElement.empty



-- View : Helpers


view :
    (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    ->
        ((W.Chart.Internal.RenderDataFull msg x y z
          -> W.Chart.Internal.RenderDataYZ x a
          -> W.Chart.Internal.DataPoint x
          -> W.Chart.Internal.DataPoint a
          -> Svg.Svg msg
         )
         -> W.Chart.ChartElement msg x y z datasets
         -> W.Chart.ChartElement msg x y z datasets
        )
    -> { toRadius : x -> ( a, Float ) -> Float }
    -> AttributesYZ msg x a
    -> W.Chart.ChartElement msg x y z datasets
view toYZ withHover props attrs =
    W.Chart.ChartElement.fromX
        (\(W.Chart.Internal.RenderData d) ->
            toYZ d
                |> Maybe.map
                    (\yzData ->
                        let
                            bubbleData : BubbleData x a
                            bubbleData =
                                toBubbleData
                                    { data = d
                                    , yzData = yzData
                                    , toRadius = props.toRadius
                                    }
                        in
                        bubbleData.points
                            |> List.map
                                (\point ->
                                    S.g
                                        [ W.Chart.Internal.attrTransformOrigin point.x.valueScaled point.yz.valueScaled
                                        , W.Chart.Internal.attrAnimationDelay d.spacings point.x.valueScaled point.yz.valueScaled
                                        , SA.class [ "ew-charts--animate-scale" ]
                                        ]
                                        [ viewBubble [] yzData attrs bubbleData point
                                        ]
                                )
                            |> W.Chart.Internal.viewTranslateChart d.spacings
                    )
                |> Maybe.withDefault (H.text "")
        )
        |> withHover
            (\d yzData xPoint yzPoint ->
                let
                    color : String
                    color =
                        attrs.toColor
                            { x = xPoint
                            , yz = yzPoint
                            , yzDomain = Scale.domain yzData.scale
                            , yzColor = yzData.toColor yzPoint.datum
                            , radius = props.toRadius xPoint.datum ( yzPoint.datum, yzPoint.value )
                            , radiusDomain = bubbleData.radiusDomain
                            }

                    bubbleData : BubbleData x a
                    bubbleData =
                        toBubbleData
                            { data = d
                            , yzData = yzData
                            , toRadius = props.toRadius
                            }
                in
                viewBubble
                    [ SA.opacity (ST.Opacity 0.5)
                    , W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 4.0
                        , color = color
                        }
                    ]
                    yzData
                    attrs
                    bubbleData
                    { x = xPoint
                    , yz = yzPoint
                    , radius = props.toRadius xPoint.datum ( yzPoint.datum, yzPoint.value )
                    }
            )


viewBubble :
    List (Svg.Attribute msg)
    -> W.Chart.Internal.RenderDataYZ x a
    -> AttributesYZ msg x a
    -> BubbleData x a
    ->
        { x : W.Chart.Internal.DataPoint x
        , yz : W.Chart.Internal.DataPoint a
        , radius : Float
        }
    -> Svg.Svg msg
viewBubble svgAttrs yzData attrs bubbleData point =
    let
        color : String
        color =
            attrs.toColor
                { x = point.x
                , yz = point.yz
                , yzDomain = Scale.domain yzData.scale
                , yzColor = yzData.toColor point.yz.datum
                , radius = point.radius
                , radiusDomain = bubbleData.radiusDomain
                }
    in
    W.Svg.Circle.view
        (svgAttrs
            ++ [ Svg.Attributes.fill Theme.baseBackground
               , SA.fillOpacity (ST.Opacity 0.6)
               , Svg.Attributes.stroke color
               , Svg.Attributes.fill color
               ]
        )
        { x = point.x.valueScaled
        , y = point.yz.valueScaled
        , radius = Scale.convert bubbleData.radiusScale point.radius
        }



-- Attributes


{-| -}
type Attribute msg x y z
    = Attribute (Attributes msg x y z -> Attributes msg x y z)


type Attributes msg x y z
    = AttributesY (AttributesYZ msg x y)
    | AttributesZ (AttributesYZ msg x z)


type alias AttributesYZ msg x a =
    { toColor : PointData x a -> String
    , htmlAttrs : List (H.Attribute msg)
    }


type alias PointData x a =
    { x : W.Chart.Internal.DataPoint x
    , yz : W.Chart.Internal.DataPoint a
    , yzDomain : ( Float, Float )
    , yzColor : String
    , radiusDomain : ( Float, Float )
    , radius : Float
    }


type alias BubbleData x a =
    { points :
        List
            { x : W.Chart.Internal.DataPoint x
            , yz : W.Chart.Internal.DataPoint a
            , radius : Float
            }
    , radiusScale : Scale.ContinuousScale Float
    , radiusDomain : ( Float, Float )
    }


applyAttrs : (AttributesYZ msg x a -> Attributes msg x y z) -> List (Attribute msg x y z) -> Attributes msg x y z
applyAttrs toDefaultAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) (toDefaultAttrs defaultAttrs) attrs


defaultAttrs : AttributesYZ msg x a
defaultAttrs =
    { toColor = .yzColor
    , htmlAttrs = []
    }


toAttrsY : Attributes msg x y z -> Maybe (AttributesYZ msg x y)
toAttrsY attrs =
    case attrs of
        AttributesY a ->
            Just a

        AttributesZ _ ->
            Nothing


toAttrsZ : Attributes msg x y z -> Maybe (AttributesYZ msg x z)
toAttrsZ attrs =
    case attrs of
        AttributesY _ ->
            Nothing

        AttributesZ a ->
            Just a


toBubbleData :
    { data : W.Chart.Internal.RenderDataFull msg x y z
    , yzData : W.Chart.Internal.RenderDataYZ x a
    , toRadius : x -> ( a, Float ) -> Float
    }
    -> BubbleData x a
toBubbleData props =
    let
        minBubbleSize : Float
        minBubbleSize =
            4

        maxBubbleSize : Float
        maxBubbleSize =
            props.data.spacings.chart.height * 0.125

        points :
            List
                { x : W.Chart.Internal.DataPoint x
                , yz : W.Chart.Internal.DataPoint a
                , radius : Float
                }
        points =
            props.data.x.data
                |> List.map
                    (\x ->
                        let
                            xScaled : Float
                            xScaled =
                                Scale.convert props.data.x.scale x
                        in
                        props.yzData.data
                            |> List.filterMap
                                (\y ->
                                    props.yzData.toValue y x
                                        |> Maybe.map
                                            (\yValue ->
                                                { x =
                                                    { datum = x
                                                    , missing = False
                                                    , value = xScaled
                                                    , valueScaled = xScaled
                                                    , stackedStart = 0
                                                    , stackedEnd = 0
                                                    }
                                                , yz =
                                                    { datum = y
                                                    , missing = False
                                                    , value = yValue
                                                    , valueScaled = Scale.convert props.yzData.scale yValue
                                                    , stackedStart = 0
                                                    , stackedEnd = 0
                                                    }
                                                , radius = props.toRadius x ( y, yValue )
                                                }
                                            )
                                )
                    )
                |> List.concat

        radiusDomain : ( Float, Float )
        radiusDomain =
            points
                |> W.Chart.Internal.boundsAt .radius
                |> Maybe.map (\b -> ( b.min, b.max ))
                |> Maybe.withDefault ( 0.0, 0.0 )
    in
    { points = points
    , radiusDomain = radiusDomain
    , radiusScale =
        Scale.linear
            ( minBubbleSize, maxBubbleSize )
            radiusDomain
    }



-- Attributes : Setters


colorFromValue : (Float -> String) -> Attribute msg x y z
colorFromValue v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = \data -> v data.yz.value }

                AttributesZ attr ->
                    AttributesZ { attr | toColor = \data -> v data.yz.value }
        )


colorFromRadius : (Float -> String) -> Attribute msg x y z
colorFromRadius v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = \data -> v data.radius }

                AttributesZ attr ->
                    AttributesZ { attr | toColor = \data -> v data.radius }
        )


colorFromPercentile : (Float -> String) -> Attribute msg x y z
colorFromPercentile v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) }

                AttributesZ attr ->
                    AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) }
        )


colorFromRadiusPercentile : (Float -> String) -> Attribute msg x y z
colorFromRadiusPercentile v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = \data -> v (toValueInPercentil data.radiusDomain data.radius) }

                AttributesZ attr ->
                    AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.radiusDomain data.radius) }
        )


colorFromPercentiles : (Float -> Float -> String) -> Attribute msg x y z
colorFromPercentiles v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) (toValueInPercentil data.radiusDomain data.radius) }

                AttributesZ attr ->
                    AttributesZ { attr | toColor = \data -> v (toValueInPercentil data.yzDomain data.yz.value) (toValueInPercentil data.radiusDomain data.radius) }
        )


colorCustomY :
    ({ x : W.Chart.Internal.DataPoint x
     , yz : W.Chart.Internal.DataPoint y
     , yzDomain : ( Float, Float )
     , yzColor : String
     , radiusDomain : ( Float, Float )
     , radius : Float
     }
     -> String
    )
    -> Attribute msg x y z
colorCustomY v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY { attr | toColor = v }

                AttributesZ attr ->
                    AttributesZ attr
        )


colorCustomZ :
    ({ x : W.Chart.Internal.DataPoint x
     , yz : W.Chart.Internal.DataPoint z
     , yzDomain : ( Float, Float )
     , yzColor : String
     , radiusDomain : ( Float, Float )
     , radius : Float
     }
     -> String
    )
    -> Attribute msg x y z
colorCustomZ v =
    Attribute
        (\a ->
            case a of
                AttributesY attr ->
                    AttributesY attr

                AttributesZ attr ->
                    AttributesZ { attr | toColor = v }
        )



-- Helpers


toValueInPercentil : ( Float, Float ) -> Float -> Float
toValueInPercentil ( min, max ) value =
    (value - min) / (max - min)
