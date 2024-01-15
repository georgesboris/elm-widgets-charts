module W.Chart.Internal.ScaleTest exposing (suite)

import Expect
import Scale
import Test exposing (Test, describe, test)
import W.Chart.Internal.Scale


suite : Test
suite =
    let
        aScale : Scale.ContinuousScale Float
        aScale =
            -- 0.5 0.5
            Scale.linear ( 0, 100 ) ( -1, 1 )

        bScale : Scale.ContinuousScale Float
        bScale =
            -- 0.2 0.8
            Scale.linear ( 0, 100 ) ( 20, 80 )
    in
    describe "W.Chart.Internal.Scale normalizeLinear"
        [ test "works on positive scales" <|
            \_ ->
                Expect.all
                    [ Tuple.first >> Scale.domain >> Expect.equal ( -1, 1 )
                    , Tuple.second >> Scale.domain >> Expect.equal ( -1, 80 )
                    ]
                    (W.Chart.Internal.Scale.normalizeLinear
                        aScale
                        bScale
                    )
        ]
