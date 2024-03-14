module W.Chart.Tooltip exposing (view, viewPoints)

import Html as H
import Html.Attributes as HA
import Svg
import Svg.Attributes
import Theme
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart.Internal


viewPoints :
    W.Chart.Internal.RenderDataFull msg x y z
    -> W.Chart.Internal.ChartPointData point
    -> H.Html msg
viewPoints d pointData =
    H.div
        []
        [ H.h1
            [ HA.class "ew-charts--tooltip-x" ]
            [ H.text pointData.x.label
            ]
        , viewDataset d.attrs.yAxis pointData.y
        , viewDataset d.attrs.zAxis pointData.z
        ]


viewDataset :
    W.Chart.Internal.AxisAttributes
    -> List W.Chart.Internal.RenderDatum
    -> H.Html msg
viewDataset datasetAttrs dataPoints =
    dataPoints
        |> W.Chart.Internal.maybeIf (not << List.isEmpty)
        |> Maybe.map
            (\_ ->
                H.section
                    [ HA.class "ew-charts--tooltip-yz" ]
                    [ datasetAttrs.label
                        |> Maybe.map (\l -> H.h2 [ HA.class "ew-charts--tooltip-yz--label" ] [ H.text l ])
                        |> Maybe.withDefault (H.text "")
                    , H.ul [ HA.class "ew-charts--tooltip-yz--list" ]
                        (List.map viewItem dataPoints)
                    ]
            )
        |> Maybe.withDefault (H.text "")


viewItem : W.Chart.Internal.RenderDatum -> H.Html msg
viewItem point =
    H.li
        [ HA.class "ew-charts--tooltip-yz--item" ]
        [ H.span
            [ HA.class "ew-charts--tooltip-yz--item-color"
            , HA.style "background" point.color
            ]
            []
        , H.span [ HA.class "ew-charts--tooltip-yz--item-label" ] [ H.text point.label ]
        , H.span [ HA.class "ew-charts--tooltip-yz--item-value" ] [ H.text point.valueString ]
        ]


view :
    W.Chart.Internal.RenderDataFull msg x y z
    -> Float
    -> Float
    -> Float
    -> List (H.Html msg)
    -> SC.Svg msg
view renderData x y margin children =
    let
        alignTop : Bool
        alignTop =
            y <= renderData.spacings.chart.halfHeight

        alignLeft : Bool
        alignLeft =
            x >= renderData.spacings.chart.halfWidth

        wrapperX : Float
        wrapperX =
            if alignLeft then
                0

            else
                x + margin

        wrapperY : Float
        wrapperY =
            if alignTop then
                y

            else
                0

        wrapperWidth : Float
        wrapperWidth =
            if alignLeft then
                x - margin

            else
                renderData.spacings.chart.width - x - margin

        wrapperHeight : Float
        wrapperHeight =
            if alignTop then
                renderData.spacings.chart.height - y

            else
                y
    in
    Svg.foreignObject
        [ SA.class [ "ew-charts--tooltip-wrapper" ]
        , Svg.Attributes.width (String.fromFloat wrapperWidth ++ "px")
        , Svg.Attributes.height (String.fromFloat wrapperHeight ++ "px")
        , SAP.y wrapperY
        , SAP.x wrapperX
        ]
        [ H.div
            [ HA.attribute "xlmns" "http://www.w3.org/1999/xhtml"
            , HA.class "ew-charts--tooltip"
            , HA.classList
                [ ( "m--align-left", alignLeft )
                , ( "m--align-top", alignTop )
                ]
            , HA.style "width" (String.fromFloat wrapperWidth ++ "px")
            , HA.style "height" (String.fromFloat wrapperHeight ++ "px")
            ]
            [ H.div
                [ HA.class "ew-bg-base-bg ew-text-base-fg"
                , HA.class "ew-border ew-border-solid"
                , HA.class "ew-rounded ew-shadow"
                , Theme.styles
                    [ ( "border-color", Theme.baseForegroundWithAlpha 0.1 )
                    ]
                ]
                children
            ]
        ]
