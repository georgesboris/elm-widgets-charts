module W.Chart.Line exposing
    ( fromY, fromZ
    , smooth, dashed, areaAlways, lineAlways, Attribute
    )

{-|

@docs fromY, fromZ

@docs smooth, dashed, areaAlways, lineAlways, Attribute

-}

import Attr
import Path
import Shape
import SubPath
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes
import W.Svg.Circle


{-| -}
fromY : List Attribute -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY (\ctx -> viewLines attrs ctx.y ctx.points.y)
                |> W.Chart.Widget.withHover (\_ data -> viewHover data.x data.y)
        )


{-| -}
fromZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ (\ctx -> viewLines attrs ctx.z ctx.points.z)
                |> W.Chart.Widget.withHover (\_ data -> viewHover data.x data.z)
        )



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { smooth : Bool
    , dashed : Bool
    , area : Maybe Bool
    }


defaultAttrs : Attributes
defaultAttrs =
    { smooth = False
    , dashed = False
    , area = Nothing
    }


{-| -}
smooth : Attribute
smooth =
    Attr.attr (\attr -> { attr | smooth = True })


{-| -}
dashed : Attribute
dashed =
    Attr.attr (\attr -> { attr | dashed = True })


{-| -}
lineAlways : Attribute
lineAlways =
    Attr.attr (\attr -> { attr | area = Just False })


{-| -}
areaAlways : Attribute
areaAlways =
    Attr.attr (\attr -> { attr | area = Just True })



-- Helpers


viewHover : W.Chart.Internal.RenderDatum -> List W.Chart.Internal.RenderDatum -> SC.Svg msg
viewHover x ys =
    ys
        |> List.map
            (\y ->
                W.Svg.Circle.view
                    [ Svg.Attributes.fill y.color
                    , Svg.Attributes.stroke "white"
                    , W.Svg.Attributes.dropShadow
                        { xOffset = 0
                        , yOffset = 0
                        , radius = 4.0
                        , color = y.color
                        }
                    ]
                    { x = x.valueScaled
                    , y = y.valueScaled
                    , radius = 4.0
                    }
            )
        |> S.g []


viewLines : Attributes -> W.Chart.Internal.RenderAxisYZ a -> List ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLines attrs axis axisPoints =
    let
        viewLine_ : Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
        viewLine_ =
            case attrs.area of
                Just True ->
                    viewLineWithArea

                Just False ->
                    viewLine

                Nothing ->
                    if axis.isStacked || (List.length axis.data == 1) then
                        viewLineWithArea

                    else
                        viewLine
    in
    S.g [] (List.indexedMap (viewLine_ attrs) axisPoints)


viewLineWithArea : Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLineWithArea attrs index ( chartDatum, points ) =
    let
        areaPoints : List (Maybe ( ( Float, Float ), ( Float, Float ) ))
        areaPoints =
            points
                |> List.map
                    (\( x, y ) ->
                        Just
                            ( ( x.render.valueScaled
                              , y.render.valueStart
                              )
                            , ( x.render.valueScaled
                              , y.render.valueEnd
                              )
                            )
                    )

        linePoints : List (Maybe ( Float, Float ))
        linePoints =
            List.map (Maybe.map Tuple.first) areaPoints
    in
    S.g
        []
        [ Path.element
            (Shape.area (shape attrs) areaPoints)
            [ Svg.Attributes.class "ew-charts--animate-fade"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill chartDatum.color
            , Svg.Attributes.fillOpacity "0.2"
            ]
        , Path.element
            (Shape.line (shape attrs) linePoints)
            [ Svg.Attributes.class "ew-charts--animate-h-clip"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill "transparent"
            , Svg.Attributes.strokeWidth "2px"
            , Svg.Attributes.stroke chartDatum.color
            , if attrs.dashed then
                SA.strokeDasharray "4 4"

              else
                Svg.Attributes.class ""
            ]
        ]


viewLine : Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLine attrs index ( chartDatum, points ) =
    let
        linePoints : List (Maybe ( Float, Float ))
        linePoints =
            points
                |> List.map
                    (\( x, y ) ->
                        Just
                            ( x.render.valueScaled
                            , y.render.valueStart
                            )
                    )
    in
    Path.element
        (Shape.line (shape attrs) linePoints)
        [ Svg.Attributes.class "ew-charts--animate-h-clip"
        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.stroke chartDatum.color
        , if attrs.dashed then
            SA.strokeDasharray "4 4"

          else
            Svg.Attributes.class ""
        ]


shape : Attributes -> List ( Float, Float ) -> SubPath.SubPath
shape attrs =
    if attrs.smooth then
        Shape.monotoneInXCurve

    else
        Shape.linearCurve
