module Main exposing (main)

import Html as H
import Html.Attributes as HA
import Set exposing (Set)
import W.Chart
import W.Chart.Colors
import W.Container
import W.Heading
import W.Styles
import W.Text


main : H.Html msg
main =
    H.div
        []
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , W.Chart.globalStyles
        , H.div
            [ HA.style "gap" "16px"
            , HA.style "padding" "16px"
            , HA.style "background" "#eee"
            , HA.style "display" "grid"
            , HA.style "grid-template-columns" "repeat(2, minmax(0, 1fr))"
            ]
            [ viewPaletteList
                [ ( "Cool", W.Chart.Colors.cool )
                , ( "Warm", W.Chart.Colors.warm )
                , ( "Rainbow", W.Chart.Colors.rainbow )
                , ( "Contrast", W.Chart.Colors.contrast )
                ]
            , viewPaletteList
                [ ( "Rose", W.Chart.Colors.rose )
                , ( "Red", W.Chart.Colors.red )
                , ( "Orange", W.Chart.Colors.orange )
                , ( "Amber", W.Chart.Colors.amber )
                , ( "Yellow", W.Chart.Colors.yellow )
                , ( "Lime", W.Chart.Colors.lime )
                , ( "Green", W.Chart.Colors.green )
                , ( "Emerald", W.Chart.Colors.emerald )
                , ( "Teal", W.Chart.Colors.teal )
                , ( "Cyan", W.Chart.Colors.cyan )
                , ( "Sky", W.Chart.Colors.sky )
                , ( "Blue", W.Chart.Colors.blue )
                , ( "Indigo", W.Chart.Colors.indigo )
                , ( "Violet", W.Chart.Colors.violet )
                , ( "Purple", W.Chart.Colors.purple )
                , ( "Pink", W.Chart.Colors.pink )
                , ( "Gray", W.Chart.Colors.gray )
                ]
            ]
        ]


viewPaletteList : List ( String, W.Chart.Colors.Palette ) -> H.Html msg
viewPaletteList paletteList =
    paletteList
        |> List.map viewPalette
        |> W.Container.view [ W.Container.gap_2 ]


viewPalette : ( String, Int -> String ) -> H.Html msg
viewPalette ( name, palette ) =
    let
        colors : List String
        colors =
            toColors palette

        colorsCount : String
        colorsCount =
            colors
                |> List.length
                |> String.fromInt
    in
    W.Container.view
        [ W.Container.card
        , W.Container.pad_2
        , W.Container.gap_1
        ]
        [ W.Container.view
            [ W.Container.horizontal
            , W.Container.alignTop
            , W.Container.gap_1
            ]
            [ W.Heading.view
                [ W.Heading.extraSmall
                , W.Heading.htmlAttrs [ HA.style "line-height" "1em" ]
                ]
                [ H.text name ]
            , W.Text.view
                [ W.Text.extraSmall
                , W.Text.aux
                , W.Text.htmlAttrs [ HA.style "line-height" "1em" ]
                ]
                [ H.text colorsCount, H.text " colors" ]
            ]
        , W.Container.view [ W.Container.gap_1 ]
            (List.map viewColor colors)
        ]


toColors : (Int -> String) -> List String
toColors palette =
    toColorsHelper palette 0 Set.empty []
        |> List.reverse


toColorsHelper : (Int -> String) -> Int -> Set String -> List String -> List String
toColorsHelper palette index set acc =
    let
        nextColor : String
        nextColor =
            palette index

        colorIsRepeated : Bool
        colorIsRepeated =
            Set.member nextColor set
    in
    if colorIsRepeated then
        acc

    else
        toColorsHelper
            palette
            (index + 1)
            (Set.insert nextColor set)
            (nextColor :: acc)


viewColor : String -> H.Html msg
viewColor color =
    H.div
        [ HA.style "background" color
        , HA.style "border-radius" "4px"
        , HA.style "height" "12px"
        ]
        []
