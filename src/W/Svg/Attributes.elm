module W.Svg.Attributes exposing
    ( dropShadow
    , filterDropShadow
    )

import Svg
import Svg.Attributes


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
