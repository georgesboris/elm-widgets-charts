module W.Chart.Colors exposing (..)

{-| Accessible colors based on <https://www.s-ings.com/scratchpad/oklch-smooth/> .
-}

import Dict


colorsCount : Int
colorsCount =
    17


shadesCount : Int
shadesCount =
    9


colorDefault : String
colorDefault =
    "#fce9ec"


colors : Dict.Dict ( Int, Int ) String
colors =
    Dict.fromList
        [ ( ( 0, 0 ), "#fce9ec" )
        , ( ( 0, 1 ), "#ffd2da" )
        , ( ( 0, 2 ), "#ffb7c5" )
        , ( ( 0, 3 ), "#ff93ab" )
        , ( ( 0, 4 ), "#fc6e91" )
        , ( ( 0, 5 ), "#e74b77" )
        , ( ( 0, 6 ), "#be2f5c" )
        , ( ( 0, 7 ), "#911541" )
        , ( ( 0, 8 ), "#5a0023" )
        , ( ( 1, 0 ), "#fde9e7" )
        , ( ( 1, 1 ), "#fed4d0" )
        , ( ( 1, 2 ), "#ffb9b4" )
        , ( ( 1, 3 ), "#ff9793" )
        , ( ( 1, 4 ), "#ff6c6f" )
        , ( ( 1, 5 ), "#ed4752" )
        , ( ( 1, 6 ), "#c7263a" )
        , ( ( 1, 7 ), "#970823" )
        , ( ( 1, 8 ), "#5c0010" )
        , ( ( 2, 0 ), "#feeae3" )
        , ( ( 2, 1 ), "#ffd4c4" )
        , ( ( 2, 2 ), "#ffbba1" )
        , ( ( 2, 3 ), "#ff9b76" )
        , ( ( 2, 4 ), "#fc784a" )
        , ( ( 2, 5 ), "#e45729" )
        , ( ( 2, 6 ), "#be3c0f" )
        , ( ( 2, 7 ), "#8e2500" )
        , ( ( 2, 8 ), "#551400" )
        , ( ( 3, 0 ), "#ffecdb" )
        , ( ( 3, 1 ), "#ffdcb9" )
        , ( ( 3, 2 ), "#ffc587" )
        , ( ( 3, 3 ), "#ffaa4a" )
        , ( ( 3, 4 ), "#fa9016" )
        , ( ( 3, 5 ), "#e17900" )
        , ( ( 3, 6 ), "#b66000" )
        , ( ( 3, 7 ), "#7d4200" )
        , ( ( 3, 8 ), "#472500" )
        , ( ( 4, 0 ), "#feedc6" )
        , ( ( 4, 1 ), "#ffe197" )
        , ( ( 4, 2 ), "#ffd158" )
        , ( ( 4, 3 ), "#fdc215" )
        , ( ( 4, 4 ), "#edaf00" )
        , ( ( 4, 5 ), "#cb9300" )
        , ( ( 4, 6 ), "#9e7100" )
        , ( ( 4, 7 ), "#6d4d00" )
        , ( ( 4, 8 ), "#3d2a00" )
        , ( ( 5, 0 ), "#eef2d4" )
        , ( ( 5, 1 ), "#e3ed9f" )
        , ( ( 5, 2 ), "#d4e360" )
        , ( ( 5, 3 ), "#c1d126" )
        , ( ( 5, 4 ), "#aab900" )
        , ( ( 5, 5 ), "#8e9b00" )
        , ( ( 5, 6 ), "#6f7900" )
        , ( ( 5, 7 ), "#4f5600" )
        , ( ( 5, 8 ), "#2d3100" )
        , ( ( 6, 0 ), "#e1f5d8" )
        , ( ( 6, 1 ), "#caf1b9" )
        , ( ( 6, 2 ), "#a7e788" )
        , ( ( 6, 3 ), "#87d65d" )
        , ( ( 6, 4 ), "#6dbe3d" )
        , ( ( 6, 5 ), "#57a126" )
        , ( ( 6, 6 ), "#418014" )
        , ( ( 6, 7 ), "#2d5c0a" )
        , ( ( 6, 8 ), "#163700" )
        , ( ( 7, 0 ), "#e0f5e5" )
        , ( ( 7, 1 ), "#c5eecf" )
        , ( ( 7, 2 ), "#96e4ad" )
        , ( ( 7, 3 ), "#5ed78a" )
        , ( ( 7, 4 ), "#2ec06d" )
        , ( ( 7, 5 ), "#1ca25a" )
        , ( ( 7, 6 ), "#148046" )
        , ( ( 7, 7 ), "#105d32" )
        , ( ( 7, 8 ), "#07371c" )
        , ( ( 8, 0 ), "#d9f6ed" )
        , ( ( 8, 1 ), "#b7eede" )
        , ( ( 8, 2 ), "#78e5c9" )
        , ( ( 8, 3 ), "#33d6b3" )
        , ( ( 8, 4 ), "#00bc9a" )
        , ( ( 8, 5 ), "#169e81" )
        , ( ( 8, 6 ), "#0b7e66" )
        , ( ( 8, 7 ), "#065b49" )
        , ( ( 8, 8 ), "#00372b" )
        , ( ( 9, 0 ), "#d7f5f6" )
        , ( ( 9, 1 ), "#b0eef0" )
        , ( ( 9, 2 ), "#70e3e8" )
        , ( ( 9, 3 ), "#33d1d8" )
        , ( ( 9, 4 ), "#1db8bf" )
        , ( ( 9, 5 ), "#0f9aa1" )
        , ( ( 9, 6 ), "#0a7a80" )
        , ( ( 9, 7 ), "#05595e" )
        , ( ( 9, 8 ), "#003538" )
        , ( ( 10, 0 ), "#daf4ff" )
        , ( ( 10, 1 ), "#b9eafe" )
        , ( ( 10, 2 ), "#84dcfe" )
        , ( ( 10, 3 ), "#4dcbf6" )
        , ( ( 10, 4 ), "#09b5e3" )
        , ( ( 10, 5 ), "#069ac2" )
        , ( ( 10, 6 ), "#077a9b" )
        , ( ( 10, 7 ), "#065871" )
        , ( ( 10, 8 ), "#003344" )
        , ( ( 11, 0 ), "#e3f1fe" )
        , ( ( 11, 1 ), "#c3e3ff" )
        , ( ( 11, 2 ), "#9fd4ff" )
        , ( ( 11, 3 ), "#6fc1ff" )
        , ( ( 11, 4 ), "#3aadfc" )
        , ( ( 11, 5 ), "#0391e6" )
        , ( ( 11, 6 ), "#0073c1" )
        , ( ( 11, 7 ), "#005391" )
        , ( ( 11, 8 ), "#002f56" )
        , ( ( 12, 0 ), "#e8eeff" )
        , ( ( 12, 1 ), "#d7e1fe" )
        , ( ( 12, 2 ), "#bdcefe" )
        , ( ( 12, 3 ), "#a0b8ff" )
        , ( ( 12, 4 ), "#7f9dff" )
        , ( ( 12, 5 ), "#6381f8" )
        , ( ( 12, 6 ), "#4a62d4" )
        , ( ( 12, 7 ), "#32449c" )
        , ( ( 12, 8 ), "#1e2959" )
        , ( ( 13, 0 ), "#f1ebff" )
        , ( ( 13, 1 ), "#e6dbff" )
        , ( ( 13, 2 ), "#d7c5fe" )
        , ( ( 13, 3 ), "#c6a9ff" )
        , ( ( 13, 4 ), "#b48aff" )
        , ( ( 13, 5 ), "#9c6bed" )
        , ( ( 13, 6 ), "#7f50ca" )
        , ( ( 13, 7 ), "#5d3996" )
        , ( ( 13, 8 ), "#361c5c" )
        , ( ( 14, 0 ), "#fce7fc" )
        , ( ( 14, 1 ), "#fbd1fb" )
        , ( ( 14, 2 ), "#f9b1fb" )
        , ( ( 14, 3 ), "#f190f5" )
        , ( ( 14, 4 ), "#e171e7" )
        , ( ( 14, 5 ), "#c458cb" )
        , ( ( 14, 6 ), "#a13da7" )
        , ( ( 14, 7 ), "#79237f" )
        , ( ( 14, 8 ), "#4c0452" )
        , ( ( 15, 0 ), "#fee8f2" )
        , ( ( 15, 1 ), "#fcd3e6" )
        , ( ( 15, 2 ), "#ffb4d9" )
        , ( ( 15, 3 ), "#ff8ac8" )
        , ( ( 15, 4 ), "#f668b6" )
        , ( ( 15, 5 ), "#dc4d9e" )
        , ( ( 15, 6 ), "#b82f7f" )
        , ( ( 15, 7 ), "#8c145d" )
        , ( ( 15, 8 ), "#580037" )
        , ( ( 16, 0 ), "#eaf0f5" )
        , ( ( 16, 1 ), "#d8e1e9" )
        , ( ( 16, 2 ), "#c1cdd8" )
        , ( ( 16, 3 ), "#a9b8c6" )
        , ( ( 16, 4 ), "#8fa1b2" )
        , ( ( 16, 5 ), "#74889b" )
        , ( ( 16, 6 ), "#5b6c7c" )
        , ( ( 16, 7 ), "#414e5a" )
        , ( ( 16, 8 ), "#262f37" )
        ]


colorOffset : Int
colorOffset =
    0


initialShadeOffset : Int
initialShadeOffset =
    1


shadeOffset : Int
shadeOffset =
    2


forIndex : Int -> String
forIndex index =
    let
        color : Int
        color =
            modBy colorsCount (index + colorOffset)

        shadeBase : Int
        shadeBase =
            modBy shadesCount ((index // shadesCount) + initialShadeOffset)

        shadeWithOffset : Int
        shadeWithOffset =
            case modBy 2 index of
                0 ->
                    Basics.max shadeBase (modBy shadesCount (shadeBase + shadeOffset))

                _ ->
                    shadeBase
    in
    Dict.get ( color, shadeWithOffset ) colors
        |> Maybe.withDefault colorDefault