module W.Chart.Line exposing (yLine, zLine)

import Html as H
import Path
import Scale
import Shape
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart.ChartElement
import W.Chart.Internal
import W.Svg.Attributes
import W.Svg.Circle


yLine : W.Chart.Internal.ChartElement msg x y z { datasets | yData : () }
yLine =
    W.Chart.ChartElement.fromY (viewLines .y)
        |> W.Chart.ChartElement.withHoverY (\_ -> viewHover)


zLine : W.Chart.Internal.ChartElement msg x y z { datasets | zData : () }
zLine =
    W.Chart.ChartElement.fromZ (viewLines .z)
        |> W.Chart.ChartElement.withHoverZ (\_ -> viewHover)



-- Helpers


viewHover :
    W.Chart.Internal.RenderDataYZ x a
    -> W.Chart.Internal.DataPoint x
    -> List (W.Chart.Internal.DataPoint a)
    -> SC.Svg msg
viewHover yzData xPoint yzPoints =
    S.g
        []
        (List.map
            (\yzPoint ->
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
            )
            yzPoints
        )


viewLines :
    (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    -> W.Chart.Internal.RenderData msg x y z datasets
    -> SC.Svg msg
viewLines toRenderDataset (W.Chart.Internal.RenderData d) =
    toRenderDataset d
        |> Maybe.map
            (\renderDataset ->
                renderDataset.bandData
                    |> List.indexedMap
                        (\index ( datum, values ) ->
                            let
                                color : String
                                color =
                                    renderDataset.toColor datum

                                areaPoints : List (Maybe ( ( Float, Float ), ( Float, Float ) ))
                                areaPoints =
                                    List.map2
                                        (\x ( low, high ) ->
                                            let
                                                xConverted : Float
                                                xConverted =
                                                    Scale.convert d.x.scale x
                                            in
                                            Just
                                                ( ( xConverted
                                                  , Scale.convert renderDataset.scale low
                                                  )
                                                , ( xConverted
                                                  , Scale.convert renderDataset.scale high
                                                  )
                                                )
                                        )
                                        d.x.data
                                        values

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
                                    , Svg.Attributes.fill color
                                    , Svg.Attributes.fillOpacity "0.2"
                                    ]
                                , Path.element
                                    (Shape.line Shape.linearCurve linePoints)
                                    [ Svg.Attributes.class "ew-charts--animate-h-clip"
                                    , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
                                    , Svg.Attributes.fill "transparent"
                                    , Svg.Attributes.strokeWidth "2px"
                                    , Svg.Attributes.stroke color
                                    ]
                                ]
                        )
                    |> W.Chart.Internal.viewTranslateChart d.spacings
            )
        |> Maybe.withDefault (H.text "")
