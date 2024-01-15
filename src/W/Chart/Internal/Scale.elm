module W.Chart.Internal.Scale exposing (normalizeLinear)

import Scale


normalizeLinear :
    Scale.ContinuousScale Float
    -> Scale.ContinuousScale Float
    -> ( Scale.ContinuousScale Float, Scale.ContinuousScale Float )
normalizeLinear a b =
    let
        aExtent : Extent
        aExtent =
            toExtent (Scale.domain a)

        bExtent : Extent
        bExtent =
            toExtent (Scale.domain b)

        ratio : Ratio
        ratio =
            toRatio aExtent bExtent
    in
    ( Scale.linear (Scale.range a) (toNormalizedExtent aExtent ratio)
    , Scale.linear (Scale.range b) (toNormalizedExtent bExtent ratio)
    )


type ScaleWeight
    = High
    | Low


type alias Extent =
    { weight : ScaleWeight
    , low : Float
    , high : Float
    , delta : Float
    }


type alias Ratio =
    { low : Float
    , high : Float
    }


toExtent : ( Float, Float ) -> Extent
toExtent ( low, high ) =
    if high > 0 && low < 0 then
        if high >= abs low then
            { weight = High
            , low = low
            , high = high
            , delta = high - low
            }

        else
            { weight = Low
            , low = low
            , high = high
            , delta = high - low
            }

    else if low < 0 then
        { weight = Low
        , low = low
        , high = 0.0
        , delta = abs low
        }

    else
        { weight = High
        , low = 0.0
        , high = high
        , delta = high
        }


toRatio : Extent -> Extent -> Ratio
toRatio a b =
    let
        high : Float
        high =
            Basics.max a.high b.high

        low : Float
        low =
            Basics.min a.low b.low

        delta : Float
        delta =
            high - low
    in
    { high = high / delta
    , low = low / delta
    }


toNormalizedExtent : Extent -> Ratio -> ( Float, Float )
toNormalizedExtent extent ratio =
    case extent.weight of
        Low ->
            ( extent.low
            , (extent.low * ratio.high) / ratio.low
            )

        High ->
            ( (extent.high * ratio.low) / ratio.high
            , extent.high
            )
