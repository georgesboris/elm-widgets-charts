module W.Chart.Internal exposing
    ( Attribute(..)
    , Attributes
    , AxisAttributes
    , AxisConfig
    , AxisType(..)
    , ChartPoint
    , ChartPointDict
    , Config(..)
    , DataAttrs
    , DataPoint
    , HoverAttrs
    , RenderData(..)
    , RenderDataFull
    , RenderDataX
    , RenderDataYZ
    , ScaleType(..)
    , Spacings
    , StackType(..)
    , Widget(..)
    , WidgetData
    , WidgetHover(..)
    , applyAttrs
    , attrAnimationDelay
    , attrAnimationDelayX
    , attrTransformOrigin
    , bounds
    , boundsAt
    , defaultAttrs
    , defaultAxisAttributes
    , isJust
    , maybeFilter
    , toAxis
    , toRenderData
    , viewHtml
    , viewTranslate
    , viewTranslateChart
    )

import Axis
import Dict exposing (values)
import Html as H
import Html.Attributes as HA
import Html.Events exposing (onMouseLeave)
import Scale
import Set
import Shape
import Statistics
import Svg
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal.Scale



-- DataPoint


type Widget msg x y z
    = Widget (WidgetData msg x y z)


type alias WidgetData msg x y z =
    { main : Maybe (RenderData msg x y z -> Svg.Svg msg)
    , background : Maybe (RenderData msg x y z -> Svg.Svg msg)
    , foreground : Maybe (RenderData msg x y z -> Svg.Svg msg)
    , hover : Maybe (WidgetHover msg x y z)
    }


type WidgetHover msg x y z
    = HoverX (RenderDataFull msg x y z -> DataPoint x -> Svg.Svg msg)
    | HoverY (RenderDataFull msg x y z -> RenderDataYZ x y -> DataPoint x -> List (DataPoint y) -> Svg.Svg msg)
    | HoverZ (RenderDataFull msg x y z -> RenderDataYZ x z -> DataPoint x -> List (DataPoint z) -> Svg.Svg msg)
    | HoverYZ (RenderDataFull msg x y z -> RenderDataYZ x y -> RenderDataYZ x z -> ChartPoint x y z -> Svg.Svg msg)



-- Constants


xAxisPadding : Float
xAxisPadding =
    22


yAxisPadding : Float
yAxisPadding =
    62



-- Types


type Config msg x y z
    = Config (ConfigData msg x y z)


type alias ConfigData msg x y z =
    { attrs : Attributes msg
    , hover : Maybe (HoverAttrs msg x y z)
    , xData : Maybe (DataAttrs x x)
    , yData : Maybe (DataAttrs x y)
    , zData : Maybe (DataAttrs x z)
    }


type alias HoverAttrs msg x y z =
    { nearest : Bool
    , tooltip : Bool
    , onClick : Maybe (ChartPoint x y z -> msg)
    , onHover : Maybe (Maybe (ChartPoint x y z) -> msg)
    , custom : List (H.Html msg)
    }


type alias DataAttrs x a =
    { data : List a
    , toLabel : a -> String
    , toColor : a -> String
    , toValue : a -> x -> Maybe Float
    }


type RenderData msg x y z
    = RenderData (RenderDataFull msg x y z)


type alias RenderDataFull msg x y z =
    { attrs : Attributes msg
    , spacings : Spacings
    , x : RenderDataX x
    , y : Maybe (RenderDataYZ x y)
    , z : Maybe (RenderDataYZ x z)
    , points : ChartPointDict x y z
    }


type alias ChartPointDict x y z =
    { byX : Dict.Dict Float (ChartPoint x y z)
    , byXY : Dict.Dict ( Float, Float ) (ChartPoint x y z)
    }


type alias ChartPoint x y z =
    { x : DataPoint x
    , ys : List (DataPoint y)
    , zs : List (DataPoint z)
    }


type alias DataPoint a =
    { datum : a
    , missing : Bool
    , value : Float
    , valueScaled : Float
    , valueStart : Float
    , valueEnd : Float
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
    , values :
        List
            { datum : a
            , color : String
            , label : String
            , domain : ( Float, Float )
            , values : List ( a, Float, ( Float, Float ) )
            , stackedValues : List ( Float, Float )
            }
    }


toRenderData : ConfigData msg x y z -> DataAttrs x x -> RenderData msg x y z
toRenderData cfg xData =
    let
        spacings : Spacings
        spacings =
            toSpacings cfg.attrs

        bandScale : Scale.BandScale x
        bandScale =
            Scale.band
                { paddingOuter = 0.0
                , paddingInner = 0.0
                , align = 0.5
                }
                ( 0, spacings.chart.width )
                xData.data

        x : RenderDataX x
        x =
            { data = xData.data
            , toLabel = xData.toLabel
            , toColor = xData.toColor
            , toValue = xData.toValue
            , bandScale = bandScale
            , scale = Scale.toRenderable xData.toLabel bandScale
            }

        yBefore : Maybe (RenderDataYZ x y)
        yBefore =
            cfg.yData
                |> Maybe.map
                    (\yData_ ->
                        toStackedData
                            { spacings = spacings
                            , xData = xData
                            , axisData = yData_
                            , axisConfig = cfg.attrs.yAxis
                            }
                    )

        zBefore : Maybe (RenderDataYZ x z)
        zBefore =
            cfg.zData
                |> Maybe.map
                    (\zData_ ->
                        toStackedData
                            { spacings = spacings
                            , xData = xData
                            , axisData = zData_
                            , axisConfig = cfg.attrs.zAxis
                            }
                    )

        -- If both X and Z are defined
        -- we normalize them so their 0.0 match.
        ( yData, zData ) =
            case ( ( yBefore, cfg.attrs.yAxis.scale ), ( zBefore, cfg.attrs.zAxis.scale ) ) of
                ( ( Just y, Linear ), ( Just z, Linear ) ) ->
                    let
                        ( yDomain_, zDomain_ ) =
                            W.Chart.Internal.Scale.normalizeDomains
                                (Scale.domain y.scale)
                                (Scale.domain z.scale)
                    in
                    ( Just { y | scale = toScale spacings cfg.attrs.yAxis yDomain_ }
                    , Just { z | scale = toScale spacings cfg.attrs.zAxis zDomain_ }
                    )

                _ ->
                    ( yBefore
                        |> Maybe.map
                            (\y ->
                                { y
                                    | scale =
                                        toScale spacings
                                            cfg.attrs.yAxis
                                            (Scale.domain y.scale)
                                }
                            )
                    , zBefore
                        |> Maybe.map
                            (\z ->
                                { z
                                    | scale =
                                        toScale spacings
                                            cfg.attrs.zAxis
                                            (Scale.domain z.scale)
                                }
                            )
                    )
    in
    RenderData
        { attrs = cfg.attrs
        , spacings = spacings
        , x = x
        , y = yData
        , z = zData
        , points = toChartPointDict x yData zData
        }


toScale : Spacings -> AxisAttributes -> ( Float, Float ) -> Scale.ContinuousScale Float
toScale spacings axisAttributes domain =
    domain
        |> toDomainWithSafety axisAttributes
        |> toScaleFn spacings axisAttributes


toScaleFn : Spacings -> AxisAttributes -> ( Float, Float ) -> Scale.ContinuousScale Float
toScaleFn spacings axisAttributes domain =
    case axisAttributes.scale of
        Linear ->
            Scale.linear ( spacings.chart.height, 0 ) domain

        Logarithmic base ->
            Scale.log base ( spacings.chart.height, 0 ) domain


toDomainWithSafety : AxisAttributes -> ( Float, Float ) -> ( Float, Float )
toDomainWithSafety axisAttributes domain =
    case Tuple.mapBoth ceiling ceiling domain of
        ( 0, 0 ) ->
            ( 0.0, 100.0 )

        _ ->
            safeBounds axisAttributes.safety domain


toChartPointDict :
    RenderDataX x
    -> Maybe (RenderDataYZ x y)
    -> Maybe (RenderDataYZ x z)
    -> ChartPointDict x y z
toChartPointDict xData maybeYData maybeZData =
    let
        pointsBase : List ( x, List ( y, Float, ( Float, Float ) ), List ( z, Float, ( Float, Float ) ) )
        pointsBase =
            case ( maybeYData, maybeZData ) of
                ( Just yData, Just zData ) ->
                    List.map3
                        (\x yForX zForX ->
                            ( x, yForX, zForX )
                        )
                        xData.data
                        (yData.values
                            |> List.map .values
                            |> transpose
                        )
                        (zData.values
                            |> List.map .values
                            |> transpose
                        )

                ( Just yData, Nothing ) ->
                    List.map2
                        (\x yForX ->
                            ( x, yForX, [] )
                        )
                        xData.data
                        (yData.values
                            |> List.map .values
                            |> transpose
                        )

                ( Nothing, Just zData ) ->
                    List.map2
                        (\x zForX ->
                            ( x, [], zForX )
                        )
                        xData.data
                        (zData.values
                            |> List.map .values
                            |> transpose
                        )

                ( Nothing, Nothing ) ->
                    List.map (\x -> ( x, [], [] )) xData.data
    in
    pointsBase
        |> List.foldl
            (\( x, ys, zs ) ( byX, byXZ ) ->
                let
                    xScaled : Float
                    xScaled =
                        Scale.convert xData.scale x

                    xBin : Float
                    xBin =
                        Scale.convert xData.bandScale x

                    yPoints : { points : List (DataPoint y), byY : Dict.Dict Float (List (DataPoint y)) }
                    yPoints =
                        toPoints x maybeYData ys

                    zPoints : { points : List (DataPoint z), byY : Dict.Dict Float (List (DataPoint z)) }
                    zPoints =
                        toPoints x maybeZData zs

                    yValues : List Float
                    yValues =
                        Dict.keys yPoints.byY
                            |> Set.fromList
                            |> Set.union (Set.fromList (Dict.keys zPoints.byY))
                            |> Set.toList

                    xPoint : DataPoint x
                    xPoint =
                        { datum = x
                        , missing = False
                        , value = xScaled
                        , valueScaled = xScaled
                        , valueStart = xBin
                        , valueEnd = xBin + Scale.bandwidth xData.bandScale
                        }
                in
                ( ( xScaled
                  , { x = xPoint
                    , ys = yPoints.points
                    , zs = zPoints.points
                    }
                  )
                    :: byX
                , yValues
                    |> List.map
                        (\yValue ->
                            ( ( xScaled, yValue )
                            , { x = xPoint
                              , ys = Dict.get yValue yPoints.byY |> Maybe.withDefault []
                              , zs = Dict.get yValue zPoints.byY |> Maybe.withDefault []
                              }
                            )
                        )
                    |> List.append byXZ
                )
            )
            ( [], [] )
        |> (\( byX, byXY ) ->
                { byX = Dict.fromList byX
                , byXY = Dict.fromList byXY
                }
           )



-- |> Dict.fromList


toPoints : x -> Maybe (RenderDataYZ x a) -> List ( a, Float, ( Float, Float ) ) -> { points : List (DataPoint a), byY : Dict.Dict Float (List (DataPoint a)) }
toPoints x maybeYZ yzPoints =
    maybeYZ
        |> Maybe.map
            (\yz ->
                let
                    points : List (DataPoint a)
                    points =
                        yzPoints
                            |> List.map
                                (\( datum, value, ( low, high ) ) ->
                                    { datum = datum
                                    , value = value
                                    , missing = yz.toValue datum x == Nothing
                                    , valueScaled = Scale.convert yz.scale (high - low)
                                    , valueStart = Scale.convert yz.scale high
                                    , valueEnd = Scale.convert yz.scale low
                                    }
                                )
                in
                { points = points
                , byY =
                    points
                        |> List.foldl
                            (\yzPoint acc ->
                                addToList yzPoint.valueStart yzPoint acc
                            )
                            Dict.empty
                        |> Dict.map (\_ -> List.reverse)
                }
            )
        |> Maybe.withDefault { points = [], byY = Dict.empty }



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
    { debug : Bool
    , width : Float
    , ratio : Float
    , xAxis : AxisAttributes
    , yAxis : AxisAttributes
    , zAxis : AxisAttributes
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


type alias AxisAttributes =
    { label : Maybe String
    , defaultValue : Float
    , format : Float -> String
    , safety : Float
    , ticks : Int
    , scale : ScaleType
    , stackType : StackType
    , showAxis : Bool
    , showGridLines : Bool
    }


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


toAxis : AxisType -> Attributes msg -> AxisAttributes
toAxis axisType attrs =
    case axisType of
        AxisX ->
            attrs.xAxis

        AxisY ->
            attrs.yAxis

        AxisZ ->
            attrs.zAxis


defaultAxisAttributes : AxisAttributes
defaultAxisAttributes =
    { label = Nothing
    , defaultValue = 0.0
    , format = String.fromFloat
    , safety = 0.1
    , ticks = 10
    , scale = Linear
    , stackType = NoStack
    , showAxis = True
    , showGridLines = True
    }


defaultAttrs : Attributes msg
defaultAttrs =
    { debug = False
    , width = 960
    , ratio = 0.5
    , xAxis = defaultAxisAttributes
    , yAxis = defaultAxisAttributes
    , zAxis = defaultAxisAttributes
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
    , axisConfig : AxisAttributes
    }
    -> RenderDataYZ x a
toStackedData props =
    let
        dataWithValues : List ( a, ( Float, Float ), List Float )
        dataWithValues =
            props.axisData.data
                |> List.map
                    (\a ->
                        let
                            values : List Float
                            values =
                                props.xData.data
                                    |> List.map
                                        (\bin ->
                                            props.axisData.toValue a bin
                                                |> Maybe.withDefault props.axisConfig.defaultValue
                                        )
                        in
                        ( a
                        , bounds values
                        , values
                        )
                    )

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
                , data = List.map (\( a, _, xs ) -> ( a, xs )) dataWithValues
                }
    in
    { data = props.axisData.data
    , bandData = List.map2 Tuple.pair stack.labels stack.values
    , toLabel = props.axisData.toLabel
    , toColor = props.axisData.toColor
    , toValue = props.axisData.toValue
    , scale = toScaleFn props.spacings props.axisConfig stack.extent
    , stack = stack
    , values =
        List.map2
            (\( a, domain, values ) stackedValues ->
                { datum = a
                , color = props.axisData.toColor a
                , label = props.axisData.toLabel a
                , domain = domain
                , values = List.map2 (\v sv -> ( a, v, sv )) values stackedValues
                , stackedValues = stackedValues
                }
            )
            dataWithValues
            stack.values
    }


transpose : List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength listOfLists) []) listOfLists


rowsLength : List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x



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
        axisConfig : AxisAttributes
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


bounds : List Float -> ( Float, Float )
bounds data =
    Statistics.extent data
        |> Maybe.withDefault ( 0, 0 )


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


viewHtml : List (Svg.Attribute msg) -> List (Svg.Svg msg) -> SC.Svg msg
viewHtml attrs children =
    Svg.foreignObject attrs
        [ H.div [ HA.attribute "xlmns" "http://www.w3.org/1999/xhtml" ] children
        ]



---


attrAnimationDelayX : Spacings -> Float -> Svg.Attribute msg
attrAnimationDelayX spacings xScaled =
    let
        -- This percentage based on both X and Y creates an offset
        -- that makes points on the lower left appear sooner
        -- than points on the upper right
        pct : Float
        pct =
            xScaled / spacings.chart.width

        -- Controls the max offset
        -- The faster points will have 0.0 offset and
        -- the lowest points will be offset by this amount
        maxDelay : Float
        maxDelay =
            0.3

        delay : Float
        delay =
            maxDelay * pct
    in
    SA.style ("animation-delay:" ++ String.fromFloat delay ++ "s")


attrAnimationDelay : Spacings -> Float -> Float -> Svg.Attribute msg
attrAnimationDelay spacings xScaled yScaled =
    let
        -- This percentage based on both X and Y creates an offset
        -- that makes points on the lower left appear sooner
        -- than points on the upper right
        pct : Float
        pct =
            0.5 * ((xScaled / spacings.chart.width) + (yScaled / spacings.chart.height))

        -- Controls the max offset
        -- The faster points will have 0.0 offset and
        -- the lowest points will be offset by this amount
        maxDelay : Float
        maxDelay =
            0.3

        delay : Float
        delay =
            maxDelay * pct
    in
    SA.style ("animation-delay:" ++ String.fromFloat delay ++ "s")


attrTransformOrigin : Float -> Float -> Svg.Attribute msg
attrTransformOrigin cx cy =
    HA.attribute
        "transform-origin"
        (String.fromFloat cx ++ " " ++ String.fromFloat cy)



---


isJust : Maybe a -> Bool
isJust m =
    m /= Nothing


maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate =
    Maybe.andThen
        (\a ->
            if predicate a then
                Just a

            else
                Nothing
        )


addToList : comparable -> v -> Dict.Dict comparable (List v) -> Dict.Dict comparable (List v)
addToList k v =
    Dict.update
        k
        (\maybeList ->
            maybeList
                |> Maybe.map (\vs -> v :: vs)
                |> Maybe.withDefault [ v ]
                |> Just
        )
