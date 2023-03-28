module Test.Debouncer.Advanced.Internal exposing (suite)


import Expect
import Test exposing (..)


import Debouncer.Advanced.Internal as Debouncer


suite : Test
suite =
  describe "Debouncer"
    [ updateSuite
    , cancelSuite
    , tryToApplySuite
    ]


updateSuite : Test
updateSuite =
  describe "update" <|
    let
      d0 =
        Debouncer.init 5

      d1 =
        Debouncer.update 10 d0
    in
    [ test "it changes the id" <|
        \_ ->
            d1.id
              |> Expect.notEqual d0.id
    , test "it changes the value" <|
        \_ ->
            d1.value
              |> Expect.equal 10
    ]


cancelSuite : Test
cancelSuite =
  describe "cancel" <|
    let
      d0 =
          Debouncer.init 5

      d1 =
          Debouncer.update 10 { d0 | lastCommittedValue = Just 10 }

      d2 =
          Debouncer.cancel d1
    in
    [ test "it changes the id" <|
        \_ ->
            d2.id
              |> Expect.notEqual d1.id
    , test "it clears the last committed value" <|
        \_ ->
            (d1.lastCommittedValue, d2.lastCommittedValue)
              |> Expect.equal (Just 10, Nothing)
    ]


tryToApplySuite : Test
tryToApplySuite =
  describe "tryToApply"
    [ describe "when ignoreLastCommittedValue is True" <|
        let
            ignoreLastCommittedValue =
                True

            d0 =
                Debouncer.init "tolst"

            d1 =
                Debouncer.update "tolsto" d0

            d2 =
                Debouncer.update "tolstoy" d1
        in
        [ describe "when the incoming ID is fresh" <|
            let
                ({ lastCommittedValue }, result) =
                    Debouncer.tryToApply ignoreLastCommittedValue d2.id String.toUpper d2
            in
            [ test "it applies the function" <|
                \_ ->
                    result
                        |> Expect.equal (Just "TOLSTOY")
            , test "it changes the last committed value" <|
                \_ ->
                    (d2.lastCommittedValue, lastCommittedValue)
                        |> Expect.equal (Nothing, Just "tolstoy")
            ]
        , describe "when the incoming ID is stale" <|
            let
                ({ lastCommittedValue }, result) =
                    Debouncer.tryToApply ignoreLastCommittedValue d1.id String.toUpper d2
            in
            [ test "it does not apply the function" <|
                \_ ->
                    result
                        |> Expect.equal Nothing
            , test "it does not change the last committed value" <|
                \_ ->
                    (d2.lastCommittedValue, lastCommittedValue)
                        |> Expect.equal (Nothing, Nothing)
            ]
        ]
    , describe "when ignoreLastCommittedValue is False" <|
        let
            ignoreLastCommittedValue =
                False
        in
        [ describe "when the incoming ID is fresh"
            [ describe "when the last committed value equals the current value" <|
                let
                    d0 =
                        Debouncer.init "tolst"

                    d1 =
                        Debouncer.update "tolsto" { d0 | lastCommittedValue = Just "tolsto" }

                    d2 =
                        Debouncer.update "tolstoy" d1

                    d3 =
                        Debouncer.update "tolsto" d2

                    ({ lastCommittedValue }, result) =
                        Debouncer.tryToApply ignoreLastCommittedValue d3.id String.toUpper d3
                in
                [ test "it does not apply the function" <|
                    \_ ->
                        result
                            |> Expect.equal Nothing
                , test "it does not change the last committed value" <|
                    \_ ->
                        (d3.lastCommittedValue, lastCommittedValue)
                            |> Expect.equal (Just "tolsto", Just "tolsto")
                ]
            , describe "when the last committed value does not equal the current value" <|
                let
                    d0 =
                        Debouncer.init "tolst"

                    d1 =
                        Debouncer.update "tolsto" d0

                    d2 =
                        Debouncer.update "tolstoy" d1

                    d3 =
                        Debouncer.update "tolsto" d2

                    ({ lastCommittedValue }, result) =
                        Debouncer.tryToApply ignoreLastCommittedValue d3.id String.toUpper d3
                in
                [ test "it applies the function" <|
                    \_ ->
                        result
                            |> Expect.equal (Just "TOLSTO")
                , test "it changes the last committed value" <|
                    \_ ->
                        (d3.lastCommittedValue, lastCommittedValue)
                            |> Expect.equal (Nothing, Just "tolsto")
                ]
            ]
        ]
    ]
