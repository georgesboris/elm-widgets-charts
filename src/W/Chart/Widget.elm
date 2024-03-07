module W.Chart.Widget exposing
    ( empty, fromX, fromY, fromZ, fromYZ
    , withBackground, withForeground, withHoverX, withHoverY, withHoverYZ, withHoverZ
    )

{-|

@docs empty, fromX, fromY, fromZ, fromYZ
@docs withBackground, withForeground, withHoverX, withHoverY, withHoverYZ, withHoverZ

-}

import Svg
import W.Chart.Internal exposing (DataPoint, Widget(..))



-- Builders


{-| -}
empty : Widget msg x y z
empty =
    Widget
        { main = Nothing
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromX : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> Widget msg x y z
fromX a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromY :
    (W.Chart.Internal.RenderData msg x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z
fromY =
    fromX


{-| -}
fromZ : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> Widget msg x y z
fromZ =
    fromX


{-| -}
fromYZ : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> Widget msg x y z
fromYZ =
    fromX



-- Options


{-| -}
withBackground :
    (W.Chart.Internal.RenderData msg x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withBackground v (Widget d) =
    Widget { d | background = Just v }


{-| -}
withForeground :
    (W.Chart.Internal.RenderData msg x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withForeground v (Widget d) =
    Widget { d | foreground = Just v }


{-| -}
withHoverX :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> DataPoint x
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withHoverX v (Widget d) =
    Widget { d | hover = Just (W.Chart.Internal.HoverX v) }


{-| -}
withHoverY :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x y
     -> DataPoint x
     -> List (DataPoint y)
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withHoverY v (Widget d) =
    Widget { d | hover = Just (W.Chart.Internal.HoverY v) }


{-| -}
withHoverZ :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x z
     -> DataPoint x
     -> List (DataPoint z)
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withHoverZ v (Widget d) =
    Widget { d | hover = Just (W.Chart.Internal.HoverZ v) }


{-| -}
withHoverYZ :
    (W.Chart.Internal.RenderDataFull msg x y z
     -> W.Chart.Internal.RenderDataYZ x y
     -> W.Chart.Internal.RenderDataYZ x z
     -> W.Chart.Internal.ChartPoint x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z
    -> Widget msg x y z
withHoverYZ v (Widget d) =
    Widget { d | hover = Just (W.Chart.Internal.HoverYZ v) }
