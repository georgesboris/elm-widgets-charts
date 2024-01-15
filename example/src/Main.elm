module Main exposing (main)

import Array
import Html as H
import Html.Attributes as HA
import W.Chart
import W.Chart.Bar
import W.Container
import W.Styles


main : H.Html msg
main =
    let
        chartConfig :
            W.Chart.Config
                msg
                Int
                ( String, String, Float -> Float )
                ()
                { with | zData : () }
        chartConfig =
            W.Chart.config []
                { data = List.range 0 4
                , toLabel = String.fromInt
                }
                |> W.Chart.withYDataset []
                    { data = [ ( "Sin", "aquamarine", Basics.sin ), ( "Cos", "blue", Basics.cos ) ]
                    , toLabel = \( label, _, _ ) -> label
                    , toColor = \( _, color, _ ) -> color
                    , toValue = \( _, _, fn ) x -> Just (fn (toFloat x))
                    }
                |> W.Chart.withZData []
                    { label = "Z Dataset"
                    , color = "pink"
                    , toValue = \x -> Array.get x (Array.fromList [ 60, -90, 8, -120, 30 ])
                    }
    in
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
            [ W.Container.view
                [ W.Container.card ]
                [ W.Chart.view
                    [ W.Chart.Bar.yBars
                    , W.Chart.Bar.zBars
                    ]
                    chartConfig
                ]
            , W.Container.view
                [ W.Container.card ]
                [ W.Chart.view [ W.Chart.Bar.yzBars ]
                    chartConfig
                ]
            ]
        ]
