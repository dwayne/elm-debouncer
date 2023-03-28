module Test.Debouncer exposing (suite)


import Expect
import Test exposing (..)


import Debouncer exposing (Delay(..))


suite : Test
suite =
  describe "Debouncer"
    [ delay
    , tryToApply
    ]


delay : Test
delay =
  describe "delay"
    [ test "it changes the wrapped value" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap 5

            (value1, _) =
              Debouncer.delay 250 10 value0
          in
          Debouncer.unwrap value1
            |> Expect.equal 10
    , test "it returns a delay effect with the correct milliseconds" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap 5

            (_, (Delay millis _)) =
              Debouncer.delay 250 10 value0
          in
          millis
            |> Expect.equal 250
    , test "it changes the id after each delay" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap 5

            (value1, (Delay _ id1)) =
              Debouncer.delay 250 10 value0

            (_, (Delay _ id2)) =
              Debouncer.delay 250 10 value1
          in
          id2
            |> Expect.notEqual id1
    ]


tryToApply : Test
tryToApply =
  let
    millis = 250
  in
  describe "tryToApply"
    [ test "it applies the function when all conditions are met" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap "tolsto"

            (value1, (Delay _ id)) =
              Debouncer.delay millis "tolstoy" value0

            (_, result) =
              Debouncer.tryToApply id identity value1
          in
          result
            |> Expect.equal (Just "tolstoy")
    , test "it does not apply the function when id is stale" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap "tolst"

            (value1, (Delay _ id1)) =
              Debouncer.delay millis "tolsto" value0

            (value2, _) =
              Debouncer.delay millis "tolstoy" value1

            (_, result) =
              Debouncer.tryToApply id1 identity value2
          in
          result
            |> Expect.equal Nothing
    , test "it does not apply the function when last committed value equals wrapped value" <|
        \_ ->
          let
            value0 =
              Debouncer.wrap "tolst"

            (value1, (Delay _ id1)) =
              Debouncer.delay millis "tolsto" value0

            -- commit to "tolsto"
            (value2, result2) =
              Debouncer.tryToApply id1 identity value1

            -- change to "tolst"
            (value3, _) =
              Debouncer.delay millis "tolst" value2

            -- but before committing to "tolst" change wrapped value back to "tolsto"
            (value4, (Delay _ id4)) =
              Debouncer.delay millis "tolsto" value3

            -- don't apply since the last committed value equals the wrapped value
            (_, result5) =
              Debouncer.tryToApply id4 identity value4
          in
          (result2, result5)
            |> Expect.equal (Just "tolsto", Nothing)
    ]
