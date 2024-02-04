module W.Chart.Internal.ScaleTest exposing (suite)

import Expect
import Fuzz
import Scale
import Test exposing (Test, describe, fuzz, skip)
import W.Chart.Internal.Scale


boundsInRange : Float -> Float -> Fuzz.Fuzzer ( Float, Float )
boundsInRange low high =
    Fuzz.pair (Fuzz.floatRange low high) (Fuzz.floatRange low high)


twoBoundsInRanges : Float -> Float -> Float -> Float -> Fuzz.Fuzzer ( ( Float, Float ), ( Float, Float ) )
twoBoundsInRanges aLow aHigh bLow bHigh =
    Fuzz.pair (boundsInRange aLow aHigh) (boundsInRange bLow bHigh)


scale : ( Float, Float ) -> Scale.ContinuousScale Float
scale =
    Scale.linear ( 0, 100 )


suite : Test
suite =
    describe "W.Chart.Internal.Scale normalizeLinear"
        [ fuzz (twoBoundsInRanges 0 100 0 100) "on positive only scales - changes nothing" <|
            \( aBounds, bBounds ) ->
                let
                    aScale : Scale.ContinuousScale Float
                    aScale =
                        scale aBounds

                    bScale : Scale.ContinuousScale Float
                    bScale =
                        scale bBounds
                in
                Expect.all
                    [ Tuple.first >> Scale.domain >> Expect.equal (Scale.domain aScale)
                    , Tuple.second >> Scale.domain >> Expect.equal (Scale.domain bScale)
                    ]
                    (W.Chart.Internal.Scale.normalizeLinear aScale aScale)
        , fuzz (twoBoundsInRanges -100 0 -100 0) "on negative only scales - changes nothing" <|
            \( aBounds, bBounds ) ->
                let
                    aScale : Scale.ContinuousScale Float
                    aScale =
                        scale aBounds

                    bScale : Scale.ContinuousScale Float
                    bScale =
                        scale bBounds
                in
                Expect.all
                    [ Tuple.first >> Scale.domain >> Expect.equal (Scale.domain aScale)
                    , Tuple.second >> Scale.domain >> Expect.equal (Scale.domain bScale)
                    ]
                    (W.Chart.Internal.Scale.normalizeLinear aScale bScale)
        , fuzz (twoBoundsInRanges -100 100 -100 100) "on mixed scales - normalizes visual zero value position" <|
            \( aBounds, bBounds ) ->
                let
                    aScale : Scale.ContinuousScale Float
                    aScale =
                        scale aBounds

                    bScale : Scale.ContinuousScale Float
                    bScale =
                        scale bBounds
                in
                Expect.equal
                    (Scale.convert aScale 0.0)
                    (Scale.convert bScale 0.0)
        ]
