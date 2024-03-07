module W.Chart.Colors exposing
    ( blues
    , colorFrom
    , cool
    , forIndex
    , grays
    , greens
    , mixed
    , purples
    , rainbow
    , reds
    , warm
    , yellows
    )

{-| Accessible colors based on <https://www.s-ings.com/scratchpad/oklch-smooth/> .
-}

import Array
import Dict



--


colorFrom : ColorList -> Int -> String
colorFrom colorList index =
    colorList
        |> getColor index



-- ColorList


type ColorList
    = ColorSingleton Color
    | ColorList Int Color (Array.Array Color)


toColorList : Color -> List Color -> ColorList
toColorList head tail =
    ColorList (List.length tail + 1) head (Array.fromList tail)


getColor : Int -> ColorList -> String
getColor i colorList =
    case colorList of
        ColorSingleton c ->
            getShade i c

        ColorList length head tail ->
            let
                i_ : Int
                i_ =
                    modBy length i

                c : Color
                c =
                    if i_ == 0 then
                        head

                    else
                        tail
                            |> Array.get (i_ - 1)
                            |> Maybe.withDefault head
            in
            getShadeContrast ((i // length) + i_) c



-- Color


type alias Color =
    { l10 : String
    , l20 : String
    , l30 : String
    , l40 : String
    , l50 : String
    , l60 : String
    , l70 : String
    , l80 : String
    , l90 : String
    }


shadesWithContrast : Array.Array (Color -> String)
shadesWithContrast =
    Array.fromList
        [ .l50
        , .l30
        , .l40
        , .l20
        ]


shadesWithContrastCount : Int
shadesWithContrastCount =
    Array.length shadesWithContrast


getShadeContrast : Int -> Color -> String
getShadeContrast i c =
    shadesWithContrast
        |> Array.get (modBy shadesWithContrastCount i)
        |> Maybe.withDefault .l40
        |> (\fn -> fn c)


colorsCount : Int
colorsCount =
    17


shadesCount : Int
shadesCount =
    9


colorDefault : String
colorDefault =
    "#fce9ec"


grays : ColorList
grays =
    toColorList colorGray []


reds : ColorList
reds =
    toColorList colorRose
        [ colorPink
        , colorRed
        , colorOrange
        ]


yellows : ColorList
yellows =
    toColorList colorAmber
        [ colorYellow
        ]


greens : ColorList
greens =
    toColorList colorLime
        [ colorGreen
        , colorEmerald
        , colorTeal
        ]


blues : ColorList
blues =
    toColorList colorCyan
        [ colorSky
        , colorBlue
        ]


purples : ColorList
purples =
    toColorList colorIndigo
        [ colorViolet
        , colorPurple
        ]


warm : ColorList
warm =
    toColorList colorRose
        [ colorRed
        , colorOrange
        , colorAmber
        , colorYellow
        , colorViolet
        , colorPurple
        , colorPink
        ]


cool : ColorList
cool =
    toColorList colorCyan
        [ colorSky
        , colorBlue
        , colorIndigo
        , colorLime
        , colorGreen
        , colorEmerald
        , colorTeal
        ]


rainbow : ColorList
rainbow =
    toColorList
        colorRose
        [ colorRed
        , colorOrange
        , colorAmber
        , colorYellow
        , colorLime
        , colorGreen
        , colorEmerald
        , colorTeal
        , colorCyan
        , colorSky
        , colorBlue
        , colorIndigo
        , colorViolet
        , colorPurple
        , colorPink
        ]


mixed : ColorList
mixed =
    toColorList
        colorRose
        [ colorBlue
        , colorRed
        , colorCyan
        , colorOrange
        , colorSky
        , colorTeal
        , colorPurple
        , colorIndigo
        , colorEmerald
        , colorAmber
        , colorYellow
        , colorViolet
        , colorLime
        , colorPink
        , colorGreen
        ]


getShade : Int -> Color -> String
getShade i c =
    case modBy shadesCount i of
        0 ->
            c.l10

        1 ->
            c.l20

        2 ->
            c.l30

        3 ->
            c.l40

        4 ->
            c.l50

        5 ->
            c.l60

        6 ->
            c.l70

        7 ->
            c.l80

        _ ->
            c.l90


colorRose : Color
colorRose =
    { l10 = "#fce9ec"
    , l20 = "#ffd2da"
    , l30 = "#ffb7c5"
    , l40 = "#ff93ab"
    , l50 = "#fc6e91"
    , l60 = "#e74b77"
    , l70 = "#be2f5c"
    , l80 = "#911541"
    , l90 = "#5a0023"
    }


colorRed : Color
colorRed =
    { l10 = "#fde9e7"
    , l20 = "#fed4d0"
    , l30 = "#ffb9b4"
    , l40 = "#ff9793"
    , l50 = "#ff6c6f"
    , l60 = "#ed4752"
    , l70 = "#c7263a"
    , l80 = "#970823"
    , l90 = "#5c0010"
    }


colorOrange : Color
colorOrange =
    { l10 = "#feeae3"
    , l20 = "#ffd4c4"
    , l30 = "#ffbba1"
    , l40 = "#ff9b76"
    , l50 = "#fc784a"
    , l60 = "#e45729"
    , l70 = "#be3c0f"
    , l80 = "#8e2500"
    , l90 = "#551400"
    }


colorAmber : Color
colorAmber =
    { l10 = "#ffecdb"
    , l20 = "#ffdcb9"
    , l30 = "#ffc587"
    , l40 = "#ffaa4a"
    , l50 = "#fa9016"
    , l60 = "#e17900"
    , l70 = "#b66000"
    , l80 = "#7d4200"
    , l90 = "#472500"
    }


colorYellow : Color
colorYellow =
    { l10 = "#feedc6"
    , l20 = "#ffe197"
    , l30 = "#ffd158"
    , l40 = "#fdc215"
    , l50 = "#edaf00"
    , l60 = "#cb9300"
    , l70 = "#9e7100"
    , l80 = "#6d4d00"
    , l90 = "#3d2a00"
    }


colorLime : Color
colorLime =
    { l10 = "#eef2d4"
    , l20 = "#e3ed9f"
    , l30 = "#d4e360"
    , l40 = "#c1d126"
    , l50 = "#aab900"
    , l60 = "#8e9b00"
    , l70 = "#6f7900"
    , l80 = "#4f5600"
    , l90 = "#2d3100"
    }


colorGreen : Color
colorGreen =
    { l10 = "#e1f5d8"
    , l20 = "#caf1b9"
    , l30 = "#a7e788"
    , l40 = "#87d65d"
    , l50 = "#6dbe3d"
    , l60 = "#57a126"
    , l70 = "#418014"
    , l80 = "#2d5c0a"
    , l90 = "#163700"
    }


colorEmerald : Color
colorEmerald =
    { l10 = "#e0f5e5"
    , l20 = "#c5eecf"
    , l30 = "#96e4ad"
    , l40 = "#5ed78a"
    , l50 = "#2ec06d"
    , l60 = "#1ca25a"
    , l70 = "#148046"
    , l80 = "#105d32"
    , l90 = "#07371c"
    }


colorTeal : Color
colorTeal =
    { l10 = "#d9f6ed"
    , l20 = "#b7eede"
    , l30 = "#78e5c9"
    , l40 = "#33d6b3"
    , l50 = "#00bc9a"
    , l60 = "#169e81"
    , l70 = "#0b7e66"
    , l80 = "#065b49"
    , l90 = "#00372b"
    }


colorCyan : Color
colorCyan =
    { l10 = "#d7f5f6"
    , l20 = "#b0eef0"
    , l30 = "#70e3e8"
    , l40 = "#33d1d8"
    , l50 = "#1db8bf"
    , l60 = "#0f9aa1"
    , l70 = "#0a7a80"
    , l80 = "#05595e"
    , l90 = "#003538"
    }


colorSky : Color
colorSky =
    { l10 = "#daf4ff"
    , l20 = "#b9eafe"
    , l30 = "#84dcfe"
    , l40 = "#4dcbf6"
    , l50 = "#09b5e3"
    , l60 = "#069ac2"
    , l70 = "#077a9b"
    , l80 = "#065871"
    , l90 = "#003344"
    }


colorBlue : Color
colorBlue =
    { l10 = "#e3f1fe"
    , l20 = "#c3e3ff"
    , l30 = "#9fd4ff"
    , l40 = "#6fc1ff"
    , l50 = "#3aadfc"
    , l60 = "#0391e6"
    , l70 = "#0073c1"
    , l80 = "#005391"
    , l90 = "#002f56"
    }


colorIndigo : Color
colorIndigo =
    { l10 = "#e8eeff"
    , l20 = "#d7e1fe"
    , l30 = "#bdcefe"
    , l40 = "#a0b8ff"
    , l50 = "#7f9dff"
    , l60 = "#6381f8"
    , l70 = "#4a62d4"
    , l80 = "#32449c"
    , l90 = "#1e2959"
    }


colorViolet : Color
colorViolet =
    { l10 = "#f1ebff"
    , l20 = "#e6dbff"
    , l30 = "#d7c5fe"
    , l40 = "#c6a9ff"
    , l50 = "#b48aff"
    , l60 = "#9c6bed"
    , l70 = "#7f50ca"
    , l80 = "#5d3996"
    , l90 = "#361c5c"
    }


colorPurple : Color
colorPurple =
    { l10 = "#fce7fc"
    , l20 = "#fbd1fb"
    , l30 = "#f9b1fb"
    , l40 = "#f190f5"
    , l50 = "#e171e7"
    , l60 = "#c458cb"
    , l70 = "#a13da7"
    , l80 = "#79237f"
    , l90 = "#4c0452"
    }


colorPink : Color
colorPink =
    { l10 = "#fee8f2"
    , l20 = "#fcd3e6"
    , l30 = "#ffb4d9"
    , l40 = "#ff8ac8"
    , l50 = "#f668b6"
    , l60 = "#dc4d9e"
    , l70 = "#b82f7f"
    , l80 = "#8c145d"
    , l90 = "#580037"
    }


colorGray : Color
colorGray =
    { l10 = "#eaf0f5"
    , l20 = "#d8e1e9"
    , l30 = "#c1cdd8"
    , l40 = "#a9b8c6"
    , l50 = "#8fa1b2"
    , l60 = "#74889b"
    , l70 = "#5b6c7c"
    , l80 = "#414e5a"
    , l90 = "#262f37"
    }


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


forIndex2 : Int -> String
forIndex2 index =
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


forIndex : Int -> String
forIndex index =
    let
        color : Int
        color =
            (index // shadesCount)
                |> modBy colorsCount

        shade : Int
        shade =
            index // shadesCount
    in
    Dict.get ( color, shade ) colors
        |> Maybe.withDefault colorDefault
