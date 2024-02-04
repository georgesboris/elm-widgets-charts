module W.Chart.Line exposing (yLine, zLine)

import Html as H
import Path
import Scale
import Shape
import Svg.Attributes
import TypedSvg.Core as SC
import W.Chart.ChartElement
import W.Chart.Internal


yLine : W.Chart.Internal.ChartElement msg x y z { datasets | yData : () }
yLine =
    W.Chart.ChartElement.fromY (viewLines .y)


zLine : W.Chart.Internal.ChartElement msg x y z { datasets | zData : () }
zLine =
    W.Chart.ChartElement.fromZ (viewLines .z)



-- Helpers


viewLines :
    (W.Chart.Internal.RenderDataFull msg x y z -> Maybe (W.Chart.Internal.RenderDataYZ x a))
    -> W.Chart.Internal.RenderData msg x y z datasets
    -> SC.Svg msg
viewLines toRenderDataset (W.Chart.Internal.RenderData d) =
    toRenderDataset d
        |> Maybe.map
            (\renderDataset ->
                renderDataset.bandData
                    |> List.concatMap
                        (\( datum, values ) ->
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
                            [ Path.element
                                (Shape.area Shape.linearCurve areaPoints)
                                [ Svg.Attributes.fill color
                                , Svg.Attributes.fillOpacity "0.2"
                                ]
                            , Path.element
                                (Shape.line Shape.linearCurve linePoints)
                                [ Svg.Attributes.fill "transparent"
                                , Svg.Attributes.strokeWidth "2px"
                                , Svg.Attributes.stroke color
                                ]
                            ]
                        )
                    |> W.Chart.Internal.viewTranslateChart d.spacings
            )
        |> Maybe.withDefault (H.text "")
