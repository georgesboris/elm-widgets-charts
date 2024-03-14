module Main exposing (main)

import Browser
import Color
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Scale.Color
import Theme
import Time exposing (Month(..))
import W.Chart
import W.Chart.Bar
import W.Chart.Bubble
import W.Chart.Colors
import W.Chart.Line
import W.Styles


type alias Point =
    W.Chart.PointXYZ Date.Date Int TrigFunction


type alias Model =
    { onClick : Maybe Point
    , onHover : Maybe Point
    }


type Msg
    = OnClick Point
    | OnMouseEnter Point
    | OnMouseLeave Point


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnClick v ->
            { model | onClick = Just v }

        OnMouseEnter v ->
            { model | onHover = Just v }

        OnMouseLeave _ ->
            { model | onHover = Nothing }



-- 1. What is our data?


date : Int -> Month -> Int -> Date
date =
    Date.fromCalendarDate


dates : List Date
dates =
    List.range 1 31
        |> List.map (\i -> Date.fromCalendarDate 2023 Jan i)


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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { onClick = Nothing, onHover = Nothing }
        , update = update
        , view =
            \model ->
                let
                    chartConfig : W.Chart.ConfigXYZ Msg Date.Date Int TrigFunction
                    chartConfig =
                        W.Chart.fromXYZ []
                            { x =
                                W.Chart.xAxis []
                                    { data = dates
                                    , toLabel = Date.format "MMM d"
                                    }
                            , y =
                                W.Chart.axisList [ W.Chart.stacked ]
                                    { data = List.range 0 9
                                    , toLabel = String.fromInt
                                    , toColor = W.Chart.Colors.colorFrom W.Chart.Colors.rainbow
                                    , toValue = \_ -> purchasesByDay
                                    }
                            , z =
                                W.Chart.axisList []
                                    { data = [ Cos, Sin ]
                                    , toLabel = trigFnLabel
                                    , toColor = trigFnColor
                                    , toValue = \fn x -> Just (applyTrigFn fn (toFloat (Date.toRataDie x)))
                                    }
                            }
                            |> W.Chart.withActive model.onClick
                            |> W.Chart.withHover
                                [ W.Chart.onClick OnClick
                                , W.Chart.onMouseEnter OnMouseEnter
                                , W.Chart.onMouseLeave OnMouseLeave
                                , W.Chart.groupByXY

                                -- , W.Chart.noTooltip
                                ]
                in
                viewWrapper model
                    [ chartConfig
                        |> W.Chart.view
                            [ W.Chart.Line.fromY [ W.Chart.Line.smooth, W.Chart.Line.lineAlways, W.Chart.Line.dashed ]
                            ]
                    , chartConfig
                        |> W.Chart.view
                            [ W.Chart.Bar.fromZ []
                            ]
                    , chartConfig
                        |> W.Chart.view
                            [ W.Chart.Line.fromZ []
                            , W.Chart.Bubble.fromY []
                                { toRadius = \x y -> y.render.value
                                , toColor = \x y -> y.render.color
                                }
                            ]
                    ]
        }



--


type TrigFunction
    = Cos
    | Sin


applyTrigFn : TrigFunction -> Float -> Float
applyTrigFn v =
    case v of
        Cos ->
            \x -> cos (x * 0.5)

        Sin ->
            \x -> sin (x * 0.5)


trigFnLabel : TrigFunction -> String
trigFnLabel v =
    case v of
        Cos ->
            "Cos"

        Sin ->
            "Sin"


trigFnColor : TrigFunction -> String
trigFnColor v =
    let
        index : Int
        index =
            case v of
                Cos ->
                    0

                Sin ->
                    1
    in
    W.Chart.Colors.colorFrom W.Chart.Colors.purples index


type alias Data =
    { label : String
    , color : String
    , toValue : Float -> Float
    }


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


viewWrapper : Model -> List (H.Html msg) -> H.Html msg
viewWrapper model children =
    H.div
        [ HA.style "background" Theme.baseBackground
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        ]
        [ W.Styles.globalStyles
        , Theme.globalProvider Theme.darkTheme
        , W.Chart.globalStyles
        , globalStyles
        , viewColor ( 20, 20 ) model.onClick
        , viewColor ( 80, 20 ) model.onHover
        , children
            |> List.map
                (\c ->
                    H.div
                        [ HA.style "background" Theme.baseBackground
                        , HA.style "border-radius" "8px"
                        , HA.style "box-shadow" "0 0 20px rgba(0,0,0,0.2)"
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


globalStyles : H.Html msg
globalStyles =
    H.node "style"
        []
        [ H.text """

.ew-charts--tooltip {
    font-family: var(--theme-font-text), sans-serif;
}
"""
        ]


viewColor : ( Float, Float ) -> Maybe Point -> H.Html msg
viewColor ( top, right ) maybeColor =
    maybeColor
        |> Maybe.andThen (List.head << .y)
        |> Maybe.map
            (\y ->
                H.div
                    [ HA.style "width" "40px"
                    , HA.style "height" "40px"
                    , HA.style "border-radius" "20px"
                    , HA.style "position" "fixed"
                    , HA.style "background" y.color
                    , HA.style "top" (String.fromFloat top ++ "px")
                    , HA.style "right" (String.fromFloat right ++ "px")
                    ]
                    []
            )
        |> Maybe.withDefault (H.text "")



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
