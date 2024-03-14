module W.Chart.Widget exposing
    ( empty, fromX, fromY, fromZ, fromYZ
    , withBackground, withForeground, withHover
    )

{-|

@docs empty, fromX, fromY, fromZ, fromYZ
@docs withBackground, withForeground, withHover

-}

import Svg
import W.Chart
import W.Chart.Internal exposing (Widget(..))



-- Builders


{-| -}
empty : Widget msg x y z point
empty =
    Widget
        { main = Nothing
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromX : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> W.Chart.WidgetX msg x y z a
fromX a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromY : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> W.Chart.WidgetXY msg x y z a
fromY a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromZ : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> W.Chart.WidgetXYZ msg x y z a
fromZ a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }


{-| -}
fromYZ : (W.Chart.Internal.RenderData msg x y z -> Svg.Svg msg) -> W.Chart.WidgetXYZ msg x y z a
fromYZ a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        }



-- Options


{-| -}
withBackground :
    (W.Chart.Internal.RenderData msg x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z point
    -> Widget msg x y z point
withBackground v (Widget d) =
    Widget { d | background = Just v }


{-| -}
withForeground :
    (W.Chart.Internal.RenderData msg x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z point
    -> Widget msg x y z point
withForeground v (Widget d) =
    Widget { d | foreground = Just v }


{-| -}
withHover :
    (W.Chart.RenderContext x y z -> W.Chart.Internal.ChartPointData point -> Svg.Svg msg)
    -> Widget msg x y z point
    -> Widget msg x y z point
withHover v (Widget d) =
    Widget { d | hover = Just v }
