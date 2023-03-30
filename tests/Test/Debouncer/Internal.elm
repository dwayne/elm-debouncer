module Test.Debouncer.Internal exposing (suite)


import Expect
import Test exposing (..)


import Debouncer.Internal as Debouncer


suite : Test
suite =
  describe "Debouncer"
    [ updateSuite
    , tryToApplySuite
    ]


updateSuite : Test
updateSuite =
  describe "update" <|
    let
      d0 =
        Debouncer.init

      d1 =
        Debouncer.update d0
    in
    [ test "it changes the id" <|
        \_ ->
            d1.id
              |> Expect.notEqual d0.id
    ]


tryToApplySuite : Test
tryToApplySuite =
  describe "tryToApply" <|
    let
        d0 =
            Debouncer.init

        d1 =
            Debouncer.update d0

        d2 =
            Debouncer.update d1
    in
    [ describe "when the incoming ID is fresh" <|
        let
            result =
                Debouncer.tryToApply d2.id (always 42) d2
        in
        [ test "it applies the function" <|
            \_ ->
                result
                    |> Expect.equal (Just 42)
        ]
    , describe "when the incoming ID is stale" <|
        let
            result =
                Debouncer.tryToApply d1.id (always 42) d2
        in
        [ test "it does not apply the function" <|
            \_ ->
                result
                    |> Expect.equal Nothing
        ]
    ]
