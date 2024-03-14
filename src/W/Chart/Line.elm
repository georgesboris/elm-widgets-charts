module W.Chart.Line exposing (yLine, zLine)

{-|

@docs yLine, zLine

-}

import Path
import Shape
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Svg.Attributes
import W.Svg.Circle


{-| -}
yLine : W.Chart.WidgetXY msg x y z a
yLine =
    W.Chart.Widget.fromY (\(W.Chart.Internal.RenderData d) -> viewLines .y d.ctx)
        |> W.Chart.Widget.withHover (\_ data -> viewHover data.x data.y)


{-| -}
zLine : W.Chart.WidgetXYZ msg x y z a
zLine =
    W.Chart.Widget.fromZ (\(W.Chart.Internal.RenderData d) -> viewLines .z d.ctx)
        |> W.Chart.Widget.withHover (\_ data -> viewHover data.x data.z)



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


viewLines : (W.Chart.Internal.ChartPointDict x y z -> List ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) )) -> W.Chart.Internal.RenderContext x y z -> SC.Svg msg
viewLines toData ctx =
    toData ctx.points
        |> List.indexedMap
            (\index ( chartDatum, points ) ->
                let
                    areaPoints : List (Maybe ( ( Float, Float ), ( Float, Float ) ))
                    areaPoints =
                        points
                            |> List.map (\( x, y ) -> Just ( ( x.render.valueScaled, y.render.valueEnd ), ( x.render.valueScaled, y.render.valueStart ) ))

                    linePoints : List (Maybe ( Float, Float ))
                    linePoints =
                        List.map (Maybe.map Tuple.second) areaPoints
                in
                S.g
                    []
                    [ Path.element
                        (Shape.area Shape.linearCurve areaPoints)
                        [ Svg.Attributes.class "ew-charts--animate-fade"
                        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
                        , Svg.Attributes.fill chartDatum.color
                        , Svg.Attributes.fillOpacity "0.2"
                        ]
                    , Path.element
                        (Shape.line Shape.linearCurve linePoints)
                        [ Svg.Attributes.class "ew-charts--animate-h-clip"
                        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
                        , Svg.Attributes.fill "transparent"
                        , Svg.Attributes.strokeWidth "2px"
                        , Svg.Attributes.stroke chartDatum.color
                        ]
                    ]
            )
        |> S.g []
