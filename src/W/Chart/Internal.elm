module W.Chart.Internal exposing
    ( Attribute(..)
    , Attributes
    , AxisConfig
    , AxisType(..)
    , Config(..)
    , DataAttrs
    , Element(..)
    , ElementData
    , RenderData(..)
    , RenderDataFull
    , RenderDataX
    , RenderDataYZ
    , ScaleType(..)
    , Spacings
    , StackType(..)
    , applyAttrs
    , toAxis
    , toRenderData
    , viewTranslate
    , viewTranslateChart
    )

import Axis
import Html as H
import Scale
import Shape
import Svg
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal.Scale


type Element msg x y z constraints
    = Element (ElementData msg x y z constraints)


type alias ElementData msg x y z constraints =
    { main : Maybe (RenderData msg x y z constraints -> Svg.Svg msg)
    , foreground : Maybe (RenderData msg x y z constraints -> Svg.Svg msg)
    }



-- Constants


lineStrokeWidth : Float
lineStrokeWidth =
    2


xAxisPadding : Float
xAxisPadding =
    22


yAxisPadding : Float
yAxisPadding =
    62


labelFontSize : Float
labelFontSize =
    13



-- Types


type Config msg x y z constraints
    = Config
        { attrs : List (Attribute msg)
        , xData : DataAttrs x x
        , yData : Maybe (DataAttrs x y)
        , zData : Maybe (DataAttrs x z)
        }


type alias DataAttrs x a =
    { data : List a
    , toLabel : a -> String
    , toColor : a -> String
    , toValue : a -> x -> Maybe Float
    }


type RenderData msg x y z constraints
    = RenderData (RenderDataFull msg x y z)


type alias RenderDataFull msg x y z =
    { attrs : Attributes msg
    , spacings : Spacings
    , x : RenderDataX x
    , y : Maybe (RenderDataYZ x y)
    , z : Maybe (RenderDataYZ x z)
    }


type alias RenderDataX x =
    { data : List x
    , toLabel : x -> String
    , toColor : x -> String
    , toValue : x -> x -> Maybe Float
    , scale : Axis.RenderableScale {} (List x) ( Float, Float ) x
    , bandScale : Scale.BandScale x
    }


type alias RenderDataYZ x a =
    { data : List a
    , toLabel : a -> String
    , toColor : a -> String
    , toValue : a -> x -> Maybe Float
    , scale : Scale.ContinuousScale Float
    , stack : Shape.StackResult a
    , bandData : List ( a, List ( Float, Float ) )
    }


toRenderData : Config msg x y z constraints -> RenderData msg x y z constraints
toRenderData (Config cfg) =
    let
        attrs : Attributes msg
        attrs =
            applyAttrs cfg.attrs

        spacings : Spacings
        spacings =
            toSpacings attrs

        bandScale : Scale.BandScale x
        bandScale =
            Scale.band
                { paddingOuter = attrs.binPaddingOuter
                , paddingInner = attrs.binPaddingOuter
                , align = 0.5
                }
                ( 0, spacings.chart.width )
                cfg.xData.data

        x : RenderDataX x
        x =
            { data = cfg.xData.data
            , toLabel = cfg.xData.toLabel
            , toColor = cfg.xData.toColor
            , toValue = cfg.xData.toValue
            , bandScale = bandScale
            , scale = Scale.toRenderable cfg.xData.toLabel bandScale
            }

        yBefore : Maybe (RenderDataYZ x y)
        yBefore =
            cfg.yData
                |> Maybe.map
                    (\yData ->
                        toStackedData
                            { spacings = spacings
                            , xData = cfg.xData
                            , axisData = yData
                            , axisConfig = attrs.yAxis
                            }
                    )

        zBefore : Maybe (RenderDataYZ x z)
        zBefore =
            cfg.zData
                |> Maybe.map
                    (\zData ->
                        toStackedData
                            { spacings = spacings
                            , xData = cfg.xData
                            , axisData = zData
                            , axisConfig = attrs.zAxis
                            }
                    )

        -- If both X and Z are defined
        -- we normalize them so their 0.0 match.
        ( yNormalized, zNormalized ) =
            case ( ( yBefore, zBefore ), ( attrs.yAxis.scale, attrs.zAxis.scale ) ) of
                ( ( Just y, Just z ), ( Linear, Linear ) ) ->
                    let
                        ( yScale, zScale ) =
                            W.Chart.Internal.Scale.normalizeLinear y.scale z.scale
                    in
                    ( Just { y | scale = yScale }
                    , Just { z | scale = zScale }
                    )

                _ ->
                    ( yBefore, zBefore )
    in
    RenderData
        { attrs = attrs
        , spacings = spacings
        , x = x
        , y = yNormalized
        , z = zNormalized
        }



-- Spacings


type alias Spacings =
    { canvas :
        { width : Float
        , height : Float
        , halfWidth : Float
        , halfHeight : Float
        }
    , chart :
        { width : Float
        , height : Float
        , halfWidth : Float
        , halfHeight : Float
        }
    , padding :
        { top : Float
        , bottom : Float
        , left : Float
        , right : Float
        }
    }


type Attribute msg
    = Attribute (Attributes msg -> Attributes msg)


type alias Attributes msg =
    { width : Float
    , ratio : Float
    , xAxis : AxisConfig
    , yAxis : AxisConfig
    , zAxis : AxisConfig
    , padding : Float
    , binPaddingOuter : Float
    , binPaddingInner : Float
    , background : String
    , htmlAttributes : List (H.Attribute msg)
    }


type ScaleType
    = Linear
    | Logarithmic Float


type StackType
    = NoStack
    | AbsoluteStack
    | RelativeStack


type AxisType
    = AxisX
    | AxisY
    | AxisZ


type alias AxisConfig =
    { label : Maybe String
    , default : Float
    , format : Float -> String
    , safety : Float
    , scale : ScaleType
    , stackType : StackType
    , ticks : Int
    , showAxis : Bool
    , showGrid : Bool
    }


toAxis : AxisType -> Attributes msg -> AxisConfig
toAxis axisType attrs =
    case axisType of
        AxisX ->
            attrs.xAxis

        AxisY ->
            attrs.yAxis

        AxisZ ->
            attrs.zAxis


defaultAxisConfig : AxisConfig
defaultAxisConfig =
    { label = Nothing
    , default = 0.0
    , format = String.fromFloat
    , safety = 0.25
    , ticks = 10
    , scale = Linear
    , stackType = NoStack
    , showAxis = True
    , showGrid = False
    }


defaultAttrs : Attributes msg
defaultAttrs =
    { width = 1200
    , ratio = 0.5
    , xAxis = { defaultAxisConfig | label = Just "x", showGrid = True }
    , yAxis = { defaultAxisConfig | label = Just "y", showGrid = True }
    , zAxis = { defaultAxisConfig | label = Just "z" }
    , padding = 40
    , binPaddingOuter = 0.5
    , binPaddingInner = 0.2
    , background = "transparent"
    , htmlAttributes = []
    }


applyAttrs : List (Attribute msg) -> Attributes msg
applyAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) defaultAttrs attrs



-- Helpers : StackedData


toStackedData :
    { spacings : Spacings
    , xData : DataAttrs x x
    , axisData : DataAttrs x a
    , axisConfig : AxisConfig
    }
    ->
        { data : List a
        , bandData : List ( a, List ( Float, Float ) )
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        , scale : Scale.ContinuousScale Float
        , stack : Shape.StackResult a
        }
toStackedData props =
    let
        stack : Shape.StackResult a
        stack =
            let
                stackOffset : List (List ( Float, Float )) -> List (List ( Float, Float ))
                stackOffset =
                    case props.axisConfig.stackType of
                        NoStack ->
                            identity

                        AbsoluteStack ->
                            Shape.stackOffsetNone

                        RelativeStack ->
                            Shape.stackOffsetExpand
                                -- Relative stacks may contain NaN's since division by zero may occur.
                                -- To prevent this buggy behavior we need to replace NaN with 0.0 manually.
                                >> List.map
                                    (List.map
                                        (\( yLow, yHigh ) ->
                                            if isNaN yHigh then
                                                ( yLow, 0.0 )

                                            else
                                                ( yLow, yHigh )
                                        )
                                    )
            in
            Shape.stack
                { offset = stackOffset
                , order = identity
                , data =
                    props.axisData.data
                        |> List.map
                            (\a ->
                                ( a
                                , props.xData.data
                                    |> List.map
                                        (\bin ->
                                            props.axisData.toValue a bin
                                                |> Maybe.withDefault 0
                                        )
                                )
                            )
                }

        safeExtent : ( Float, Float )
        safeExtent =
            case Tuple.mapBoth ceiling ceiling stack.extent of
                ( 0, 0 ) ->
                    ( 0.0, 100.0 )

                _ ->
                    safeBounds props.axisConfig.safety stack.extent

        scale : Scale.ContinuousScale Float
        scale =
            case props.axisConfig.scale of
                Linear ->
                    Scale.linear ( props.spacings.chart.height, 0 ) safeExtent

                Logarithmic base ->
                    Scale.log base ( props.spacings.chart.height, 0 ) safeExtent
    in
    { data = props.axisData.data
    , bandData = List.map2 Tuple.pair stack.labels stack.values
    , toLabel = props.axisData.toLabel
    , toColor = props.axisData.toColor
    , toValue = props.axisData.toValue
    , scale = scale
    , stack = stack
    }



-- Helpers : Spacings


toSpacings : Attributes msg -> Spacings
toSpacings attrs =
    let
        canvas : { width : Float, height : Float }
        canvas =
            { width = attrs.width
            , height = attrs.width * attrs.ratio
            }

        chart : { width : Float, height : Float }
        chart =
            { height = canvas.height - padding.top - padding.bottom
            , width = canvas.width - padding.left - padding.right
            }

        padding :
            { top : Float
            , bottom : Float
            , left : Float
            , right : Float
            }
        padding =
            { top = attrs.padding
            , bottom = toPadding attrs AxisX
            , left = toPadding attrs AxisY
            , right = toPadding attrs AxisZ
            }
    in
    { padding = padding
    , canvas =
        { width = canvas.width
        , height = canvas.height
        , halfWidth = canvas.width * 0.5
        , halfHeight = canvas.height * 0.5
        }
    , chart =
        { width = chart.width
        , height = chart.height
        , halfWidth = chart.width * 0.5
        , halfHeight = chart.height * 0.5
        }
    }


toPadding : Attributes msg -> AxisType -> Float
toPadding attrs axisType =
    let
        axisConfig : AxisConfig
        axisConfig =
            toAxis axisType attrs
    in
    if axisConfig.showAxis then
        let
            axisPadding : Float
            axisPadding =
                if axisType == AxisX then
                    xAxisPadding

                else
                    yAxisPadding
        in
        attrs.padding + axisPadding

    else
        attrs.padding



-- Helpers : Bounds


bounds :
    { safety : Float
    , toValue : a -> Float
    }
    -> List a
    -> ( Float, Float )
bounds props data =
    data
        |> boundsAt props.toValue
        |> Maybe.withDefault { min = 0, max = 0 }
        |> (\{ min, max } -> safeBounds props.safety ( min, max ))


{-| -}
safeBounds : Float -> ( Float, Float ) -> ( Float, Float )
safeBounds safety ( min, max ) =
    let
        -- If we have the same min and max for a given axis,
        -- that problably means we only have one data point.
        -- If that happens we will use the `min` value as the delta and expand the canvas
        -- so we prevent cases where the grid is confusing since the whole axis is made of a single value.
        baseValue : Float
        baseValue =
            if max == min then
                min

            else if max > 0 && min < 0 then
                max - min

            else if max > 0 then
                max

            else
                abs min

        minSafety : Float
        minSafety =
            min - (baseValue * safety)

        maxSafety : Float
        maxSafety =
            max + (baseValue * safety)

        -- If the `minSafety` would cause the axis to show negative values,
        -- even though there are no negative data points, then we clamp it to zero.
        minClampedSafety : Float
        minClampedSafety =
            if minSafety < 0.0 && min >= 0.0 then
                0.0

            else
                minSafety

        -- Following the same logic, if the `maxSafety` would cause the axis to show positive values,
        -- even though there are no positive data points, then we clamp it to zero.
        maxClampedSafety : Float
        maxClampedSafety =
            if maxSafety > 0.0 && max <= 0.0 then
                0.0

            else
                maxSafety
    in
    ( minClampedSafety
    , maxClampedSafety
    )


boundsAt : (a -> number) -> List a -> Maybe { max : number, min : number }
boundsAt toA xs =
    case xs of
        x :: xs_ ->
            List.foldl
                (\v ( max, min ) ->
                    ( Basics.max max (toA v)
                    , Basics.min min (toA v)
                    )
                )
                ( toA x, toA x )
                xs_
                |> (\( max, min ) ->
                        { max = max
                        , min = min
                        }
                   )
                |> Just

        [] ->
            Nothing



-- Helpers : Views


viewTranslate : { x : Float, y : Float } -> List (SC.Svg msg) -> SC.Svg msg
viewTranslate props children =
    S.g [ SA.transform [ ST.Translate props.x props.y ] ]
        children


viewTranslateChart : Spacings -> List (SC.Svg msg) -> SC.Svg msg
viewTranslateChart spacings =
    viewTranslate
        { x = spacings.padding.left
        , y = spacings.padding.top
        }
