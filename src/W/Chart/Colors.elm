module W.Chart.Colors exposing
    ( contrast, rainbow, cool, warm
    , amber, blue, cyan, emerald, gray, green, indigo, lime, orange, pink, purple, red, rose, sky, teal, violet, yellow
    , Palette
    )

{-| Accessible colors based on <https://www.s-ings.com/scratchpad/oklch-smooth/> .

@docs contrast, rainbow, cool, warm
@docs amber, blue, cyan, emerald, gray, green, indigo, lime, orange, pink, purple, red, rose, sky, teal, violet, yellow
@docs Palette

-}

import Array



-- Palette


{-| -}
type alias Palette =
    Int -> String


toPalette : Color -> List Color -> Palette
toPalette primaryColor otherColors i =
    let
        colorList : ColorList
        colorList =
            toColorList primaryColor otherColors
    in
    case colorList of
        ColorSingleton c ->
            getShadeContrast i c

        ColorList length head tail ->
            let
                colorIndex : Int
                colorIndex =
                    modBy length i

                color : Color
                color =
                    if colorIndex == 0 then
                        head

                    else
                        tail
                            |> Array.get (colorIndex - 1)
                            |> Maybe.withDefault head

                shadeIndex : Int
                shadeIndex =
                    (i // length) + modBy 2 i
            in
            getShadeContrast shadeIndex color



-- Palettes


{-| -}
rose : Palette
rose =
    toPalette colorRose []


{-| -}
red : Palette
red =
    toPalette colorRed []


{-| -}
orange : Palette
orange =
    toPalette colorOrange []


{-| -}
amber : Palette
amber =
    toPalette colorAmber []


{-| -}
yellow : Palette
yellow =
    toPalette colorYellow []


{-| -}
lime : Palette
lime =
    toPalette colorLime []


{-| -}
green : Palette
green =
    toPalette colorGreen []


{-| -}
emerald : Palette
emerald =
    toPalette colorEmerald []


{-| -}
teal : Palette
teal =
    toPalette colorTeal []


{-| -}
cyan : Palette
cyan =
    toPalette colorCyan []


{-| -}
sky : Palette
sky =
    toPalette colorSky []


{-| -}
blue : Palette
blue =
    toPalette colorBlue []


{-| -}
indigo : Palette
indigo =
    toPalette colorIndigo []


{-| -}
violet : Palette
violet =
    toPalette colorViolet []


{-| -}
purple : Palette
purple =
    toPalette colorPurple []


{-| -}
pink : Palette
pink =
    toPalette colorPink []


{-| -}
gray : Palette
gray =
    toPalette colorGray []


{-| -}
warm : Palette
warm =
    toPalette colorRose
        [ colorYellow
        , colorOrange
        , colorViolet
        , colorRed
        , colorPurple
        , colorPink
        , colorAmber
        ]


{-| -}
cool : Palette
cool =
    toPalette colorCyan
        [ colorGreen
        , colorSky
        , colorEmerald
        , colorBlue
        , colorLime
        , colorTeal
        , colorIndigo
        ]


{-| -}
rainbow : Palette
rainbow =
    toPalette
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


{-| -}
contrast : Palette
contrast =
    toPalette
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



-- Colors


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



-- ColorList


type ColorList
    = ColorSingleton Color
    | ColorList Int Color (Array.Array Color)


toColorList : Color -> List Color -> ColorList
toColorList head tail =
    case tail of
        [] ->
            ColorSingleton head

        _ ->
            ColorList (List.length tail + 1) head (Array.fromList tail)



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
        [ .l60
        , .l30
        , .l50
        , .l70
        , .l20
        ]


shadesWithContrastCount : Int
shadesWithContrastCount =
    Array.length shadesWithContrast


getShadeContrast : Int -> Color -> String
getShadeContrast i c =
    shadesWithContrast
        |> Array.get (modBy shadesWithContrastCount i)
        |> Maybe.withDefault .l60
        |> (\fn -> fn c)
