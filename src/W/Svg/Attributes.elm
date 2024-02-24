module W.Svg.Attributes exposing
    ( classList
    , cond
    , dropShadow
    , filterDropShadow
    )

import Svg
import Svg.Attributes


none : Svg.Attribute msg
none =
    Svg.Attributes.class ""


classList : List ( String, Bool ) -> Svg.Attribute msg
classList xs =
    xs
        |> List.foldl
            (\( class, predicate ) acc ->
                if predicate then
                    acc ++ " " ++ class

                else
                    acc
            )
            ""
        |> Svg.Attributes.class


cond : Bool -> Svg.Attribute msg -> Svg.Attribute msg
cond predicate attr =
    if predicate then
        attr

    else
        none


dropShadow :
    { xOffset : Float
    , yOffset : Float
    , radius : Float
    , color : String
    }
    -> Svg.Attribute msg
dropShadow props =
    Svg.Attributes.style ("filter: " ++ filterDropShadow props)


filterDropShadow :
    { xOffset : Float
    , yOffset : Float
    , radius : Float
    , color : String
    }
    -> String
filterDropShadow props =
    "drop-shadow("
        ++ String.fromFloat props.xOffset
        ++ "px "
        ++ String.fromFloat props.yOffset
        ++ "px "
        ++ String.fromFloat props.radius
        ++ "px "
        ++ props.color
        ++ ")"
