module W.Chart.Internal.Voronoi exposing (view)

import Array
import BoundingBox2d
import Dict
import Html as H
import Pixels
import Point2d
import Polygon2d
import Svg
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Types as ST
import VoronoiDiagram2d
import W.Chart.Internal


view : (( Float, Float ) -> W.Chart.Internal.ChartPoint x y z -> Svg.Svg msg) -> W.Chart.Internal.RenderDataFull msg x y z -> Svg.Svg msg
view fn d =
    let
        voronoiResult :
            Result
                (VoronoiDiagram2d.Error ( ( Float, Float ), W.Chart.Internal.ChartPoint x y z ))
                (VoronoiDiagram2d.VoronoiDiagram2d ( ( Float, Float ), W.Chart.Internal.ChartPoint x y z ) Pixels.Pixels Float)
        voronoiResult =
            d.points.byXY
                |> Dict.toList
                |> Array.fromList
                |> VoronoiDiagram2d.fromVerticesBy (\( ( x, y ), _ ) -> Point2d.pixels x y)

        boundingBox : BoundingBox2d.BoundingBox2d Pixels.Pixels Float
        boundingBox =
            BoundingBox2d.from
                (Point2d.pixels 0 0)
                (Point2d.pixels d.spacings.chart.width d.spacings.chart.height)
    in
    voronoiResult
        |> Result.map (VoronoiDiagram2d.polygons boundingBox)
        |> Result.map
            (\polygons ->
                polygons
                    |> List.foldl
                        (\( ( xy, data ), polygon ) acc ->
                            S.polygon
                                [ SA.class [ "ew-charts--hover-target" ]
                                , polygon
                                    |> Polygon2d.vertices
                                    |> List.map (Point2d.toTuple Pixels.toFloat)
                                    |> SA.points
                                , Svg.Attributes.fill "transparent"
                                ]
                                []
                                :: S.g
                                    [ SA.class [ "ew-charts--hover" ] ]
                                    [ fn xy data
                                    , if d.attrs.debug then
                                        S.circle
                                            [ SAP.cx (Tuple.first xy)
                                            , SAP.cy (Tuple.second xy)
                                            , SAP.r 2.0
                                            , Svg.Attributes.fill "red"
                                            ]
                                            []

                                      else
                                        H.text ""
                                    ]
                                :: acc
                        )
                        []
                    |> S.g []
            )
        |> Result.withDefault (H.text "")
