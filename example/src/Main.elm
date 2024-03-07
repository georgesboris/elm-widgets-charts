module Main exposing (main)

import Color
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Scale.Color
import Time exposing (Month(..))
import W.Chart
import W.Chart.Bar
import W.Chart.Bubble
import W.Chart.Colors
import W.Chart.Line
import W.Styles



-- 1. What is our data?


date : Int -> Month -> Int -> Date
date =
    Date.fromCalendarDate


impressionsList : List ( Date, Float )
impressionsList =
    [ ( date 2023 Jan 1, 1000 )
    , ( date 2023 Jan 2, 2000 )
    , ( date 2023 Jan 3, 2500 )
    , ( date 2023 Jan 4, 3000 )
    , ( date 2023 Jan 5, 1800 )
    , ( date 2023 Jan 6, 1600 )
    , ( date 2023 Jan 7, 1200 )
    ]


purchasesList : List ( Date, Float )
purchasesList =
    [ ( date 2023 Jan 1, 10 )

    -- , ( date 2023 Jan 2, 7 )
    , ( date 2023 Jan 3, 13 )
    , ( date 2023 Jan 4, 20 )
    , ( date 2023 Jan 5, 24 )
    , ( date 2023 Jan 6, 18 )
    , ( date 2023 Jan 7, 12 )
    ]


products : List String
products =
    [ "Ball", "Hero", "Lego" ]


purchasesByProductList : List ( String, Date, Float )
purchasesByProductList =
    [ ( "Ball", date 2023 Jan 1, 3 )
    , ( "Ball", date 2023 Jan 2, 2 )
    , ( "Ball", date 2023 Jan 3, 5 )
    , ( "Ball", date 2023 Jan 4, 8 )
    , ( "Ball", date 2023 Jan 5, 10 )
    , ( "Ball", date 2023 Jan 6, 8 )
    , ( "Ball", date 2023 Jan 7, 6 )
    , ( "Hero", date 2023 Jan 1, 4 )
    , ( "Hero", date 2023 Jan 2, 3 )
    , ( "Hero", date 2023 Jan 3, 6 )
    , ( "Hero", date 2023 Jan 4, 8 )
    , ( "Hero", date 2023 Jan 5, 12 )
    , ( "Hero", date 2023 Jan 6, 9 )
    , ( "Hero", date 2023 Jan 7, 5 )
    , ( "Lego", date 2023 Jan 1, 3 )
    , ( "Lego", date 2023 Jan 2, 2 )
    , ( "Lego", date 2023 Jan 3, 2 )
    , ( "Lego", date 2023 Jan 4, 4 )
    , ( "Lego", date 2023 Jan 5, 2 )
    , ( "Lego", date 2023 Jan 6, 3 )
    , ( "Lego", date 2023 Jan 7, 0 )
    ]



-- Some Helpers (for now)


purchasesDict : Dict Int Float
purchasesDict =
    purchasesList
        |> List.map (Tuple.mapFirst Date.toRataDie)
        |> Dict.fromList


impressionsDict : Dict Int Float
impressionsDict =
    impressionsList
        |> List.map (Tuple.mapFirst Date.toRataDie)
        |> Dict.fromList


impressionsByDay : Date -> Maybe Float
impressionsByDay day =
    Dict.get (Date.toRataDie day) impressionsDict


purchasesByDay : Date -> Maybe Float
purchasesByDay day =
    Dict.get (Date.toRataDie day) purchasesDict


purchasesByProduct : Dict ( String, Int ) Float
purchasesByProduct =
    purchasesByProductList
        |> List.map (\( p, d, v ) -> ( ( p, Date.toRataDie d ), v ))
        |> Dict.fromList


purchasesByProductByDay : String -> Date -> Maybe Float
purchasesByProductByDay product day =
    Dict.get (Date.toRataDie day) purchasesDict


formatMoney : Float -> String
formatMoney v =
    "$" ++ String.fromFloat v



-- 2. Exploring visualizations


main : H.Html msg
main =
    let
        chartConfig : W.Chart.Config msg Date.Date Int z
        chartConfig =
            W.Chart.config []
                |> W.Chart.withHover
                    [ W.Chart.groupByXY
                    ]
                |> W.Chart.withX []
                    { data = List.map Tuple.first purchasesList
                    , toLabel = Date.format "MMM d"
                    }
                |> W.Chart.withYList
                    [ W.Chart.format formatMoney
                    , W.Chart.stacked
                    , W.Chart.axisLabel "Numbers"
                    ]
                    { data = List.range 0 19
                    , toLabel = String.fromInt
                    , toColor = W.Chart.Colors.colorFrom W.Chart.Colors.rainbow
                    , toValue = \_ -> purchasesByDay
                    }
    in
    viewWrapper
        [ chartConfig
            |> W.Chart.view
                [ W.Chart.Line.yLine
                ]
        , chartConfig
            |> W.Chart.view
                [ W.Chart.Bar.yBars
                ]
        , chartConfig
            |> W.Chart.view
                [ W.Chart.Bubble.viewY []
                    { toRadius = \_ ( _, v ) -> v
                    }
                ]
        ]



--


type alias Data =
    { label : String
    , color : String
    , toValue : Float -> Float
    }


yDataset : List Data
yDataset =
    [ ( "Cos", cos )
    , ( "Sin", sin )
    , ( "Tan", tan )
    ]
        -- [ ( "Cos", \x -> x * 2 )
        -- , ( "Sin", \x -> x * 3 )
        -- , ( "Tan", \x -> x * 4 )
        -- ]
        |> List.indexedMap
            (\index ( label, fn ) ->
                { label = label
                , color = W.Chart.Colors.forIndex index
                , toValue = fn
                }
            )


zDataset : List Data
zDataset =
    [ ( "(x-1)^2", \x -> 2 ^ (x - 1) )
    , ( "x^2", \x -> 2 ^ x )
    ]
        |> List.indexedMap
            (\index ( label, fn ) ->
                { label = label
                , color = W.Chart.Colors.forIndex (index + 3)
                , toValue = fn
                }
            )


viewWrapper : List (H.Html msg) -> H.Html msg
viewWrapper children =
    H.div
        [ HA.style "background" "#ccc"
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        ]
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , W.Chart.globalStyles
        , children
            |> List.map
                (\c ->
                    H.div
                        [ HA.style "background" "#fff"
                        , HA.style "border-radius" "8px"
                        ]
                        [ c ]
                )
            |> H.div
                [ HA.style "width" "100%"
                , HA.style "max-width" "960px"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "gap" "20px"
                ]
        ]



-- main : H.Html msg
-- main =
--     viewCharts
--         (W.Chart.config [ W.Chart.tooltipByNearest ]
--             { data = List.range 0 10
--             , toLabel = \i -> String.fromInt i ++ "px"
--             }
--             |> W.Chart.withYList
--                 [ W.Chart.axisLabel "geometry"
--                 ]
--                 { data = yDataset
--                 , toLabel = .label
--                 , toColor = .color
--                 , toValue = \{ toValue } x -> Just (toValue (toFloat x))
--                 }
--             |> W.Chart.withZList
--                 [ W.Chart.axisLabel "functions"
--                 ]
--                 { data = zDataset
--                 , toLabel = .label
--                 , toColor = .color
--                 , toValue = \{ toValue } x -> Just (toValue (toFloat x))
--                 }
--         )
--         [ [ W.Chart.Bar.yzBars ]
--         , [ W.Chart.Line.yLine, W.Chart.Line.zLine ]
-- , [ W.Chart.Bubble.viewZ
--         [ W.Chart.Bubble.colorFromRadiusPercentile
--             (Scale.Color.viridisInterpolator >> Color.toCssString)
--         ]
--         { toRadius = \x _ -> toFloat x / 20
--         }
--   , W.Chart.Bubble.viewY
--         [ W.Chart.Bubble.colorFromRadiusPercentile
--             (Scale.Color.viridisInterpolator >> Color.toCssString)
--         ]
--         { toRadius = \x _ -> toFloat x / 20
--         }
--   ]
-- ]
