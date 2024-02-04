module Main exposing (main)

import Color
import Html as H
import Html.Attributes as HA
import Scale.Color
import W.Chart
import W.Chart.Bar
import W.Chart.Bubble
import W.Chart.Colors
import W.Chart.Line
import W.Container
import W.Styles


main : H.Html msg
main =
    viewCharts
        (W.Chart.config []
            { data = List.range 0 10
            , toLabel = String.fromInt
            }
            |> W.Chart.withYDataset
                [ W.Chart.axisLabel "m2"
                ]
                { data = yDataset
                , toLabel = .label
                , toColor = .color
                , toValue = \{ toValue } x -> Just (toValue (toFloat x))
                }
            |> W.Chart.withZDataset []
                { data = zDataset
                , toLabel = .label
                , toColor = .color
                , toValue = \{ toValue } x -> Just (toValue (toFloat x))
                }
        )
        [ [ W.Chart.Line.yLine, W.Chart.Line.zLine ]
        , [ W.Chart.Bubble.viewZ
                [ W.Chart.Bubble.colorFromRadiusPercentile
                    (Scale.Color.viridisInterpolator >> Color.toCssString)
                ]
                { toRadius = \x _ -> toFloat x / 20
                }
          , W.Chart.Bubble.viewY
                [ W.Chart.Bubble.colorFromRadiusPercentile
                    (Scale.Color.viridisInterpolator >> Color.toCssString)
                ]
                { toRadius = \x _ -> toFloat x / 20
                }
          ]
        , [ W.Chart.Bar.yzBars ]
        ]


type alias Data =
    { label : String
    , color : String
    , toValue : Float -> Float
    }


yDataset : List Data
yDataset =
    [ ( "Cos", cos )
    , ( "Sin", cos )
    , ( "Tan", tan )
    ]
        |> List.indexedMap
            (\index ( label, fn ) ->
                { label = label
                , color = W.Chart.Colors.forIndex index
                , toValue = fn
                }
            )


zDataset : List Data
zDataset =
    [ ( "*2", \x -> x * 2 )
    , ( "pow2", \x -> 2 ^ x )
    ]
        |> List.indexedMap
            (\index ( label, fn ) ->
                { label = label
                , color = W.Chart.Colors.forIndex index
                , toValue = fn
                }
            )


viewCharts : W.Chart.Config msg x y z datasets -> List (List (W.Chart.ChartElement msg x y z datasets)) -> H.Html msg
viewCharts chartConfig chartElements =
    H.div
        [ HA.style "margin" "0 auto"
        , HA.style "max-width" "960px"
        , HA.style "background" "white"
        ]
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , W.Chart.globalStyles
        , W.Container.view
            [ W.Container.gap_8
            , W.Container.background "#eaeaea"
            ]
            (List.map
                (\views ->
                    W.Container.view
                        [ W.Container.card ]
                        [ W.Chart.view views chartConfig ]
                )
                chartElements
            )
        ]
