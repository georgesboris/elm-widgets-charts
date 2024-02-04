module W.Svg.Circle exposing (view)

import Svg
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC


view :
    List (Svg.Attribute msg)
    ->
        { x : Float
        , y : Float
        , radius : Float
        }
    -> SC.Svg msg
view attrs props =
    S.circle
        (attrs
            ++ [ SAP.cx props.x
               , SAP.cy props.y
               , SAP.r props.radius
               , SAP.strokeWidth 2
               ]
        )
        []
