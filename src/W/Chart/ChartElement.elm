module W.Chart.ChartElement exposing
    ( empty
    , fromX
    , fromY
    , fromYZ
    , fromZ
    , withBackground
    , withForeground
    , withHoverX
    , withHoverY
    , withHoverYZ
    , withHoverZ
    )

import Svg
import W.Chart.Internal exposing (ChartElement(..), DataPoint)



-- Builders


empty : ChartElement msg x y z datasets
empty =
    ChartElement
        { main = Nothing
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


fromX : (W.Chart.Internal.RenderData msg x y z datasets -> Svg.Svg msg) -> ChartElement msg x y z datasets
fromX a =
    ChartElement
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


fromY :
    (W.Chart.Internal.RenderData msg x y z { datasets | yData : () }
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { datasets | yData : () }
fromY =
    fromX


fromZ :
    (W.Chart.Internal.RenderData msg x y z { datasets | zData : () }
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { datasets | zData : () }
fromZ =
    fromX


fromYZ :
    (W.Chart.Internal.RenderData msg x y z { with | yData : (), zData : () }
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { with | yData : (), zData : () }
fromYZ =
    fromX



-- Options


withBackground :
    (W.Chart.Internal.RenderData msg x y z datasets
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z datasets
    -> ChartElement msg x y z datasets
withBackground v (ChartElement d) =
    ChartElement { d | background = Just v }


withForeground :
    (W.Chart.Internal.RenderData msg x y z datasets
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z datasets
    -> ChartElement msg x y z datasets
withForeground v (ChartElement d) =
    ChartElement { d | foreground = Just v }


withHoverX :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> DataPoint x
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { with | yData : () }
    -> ChartElement msg x y z { with | yData : () }
withHoverX v (ChartElement d) =
    ChartElement { d | hover = Just (W.Chart.Internal.HoverX v) }


withHoverY :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x y
     -> DataPoint x
     -> DataPoint y
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { with | yData : () }
    -> ChartElement msg x y z { with | yData : () }
withHoverY v (ChartElement d) =
    ChartElement { d | hover = Just (W.Chart.Internal.HoverY v) }


withHoverZ :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x z
     -> DataPoint x
     -> DataPoint z
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { with | zData : () }
    -> ChartElement msg x y z { with | zData : () }
withHoverZ v (ChartElement d) =
    ChartElement { d | hover = Just (W.Chart.Internal.HoverZ v) }


withHoverYZ :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x y
     -> W.Chart.Internal.RenderDataYZ x z
     -> W.Chart.Internal.ChartPoint x y z
     -> Svg.Svg msg
    )
    -> ChartElement msg x y z { with | yData : (), zData : () }
    -> ChartElement msg x y z { with | yData : (), zData : () }
withHoverYZ v (ChartElement d) =
    ChartElement { d | hover = Just (W.Chart.Internal.HoverYZ v) }
