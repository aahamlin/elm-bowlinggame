module BowlingGameTests exposing (..)

--import Fuzz exposing (Fuzzer, int, list, string)

import Array exposing (Array)
import BowlingGame exposing (Rolls, roll, score)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Bowling Game Score"
        [ test "all gutter balls" <|
            \_ ->
                let
                    pins =
                        Array.repeat 20 0
                            |> Debug.log "pins"
                in
                rollPins pins
                    |> score
                    |> Expect.equal 0
        , test "all ones" <|
            \_ ->
                let
                    pins =
                        Array.repeat 20 1
                            |> Debug.log "pins"
                in
                rollPins pins
                    |> score
                    |> Expect.equal 20
        , test "one spare" <|
            \_ ->
                let
                    pins =
                        Array.append
                            (Array.fromList [ 5, 5, 3 ])
                            (Array.repeat 17 0)
                            |> Debug.log "pins"
                in
                rollPins pins
                    |> score
                    |> Expect.equal 16
        , test "one strike" <|
            \_ ->
                let
                    pins =
                        Array.append
                            (Array.fromList [ 10, 3, 4 ])
                            (Array.repeat 16 0)
                            |> Debug.log "pins"
                in
                rollPins pins
                    |> score
                    |> Expect.equal 24
        , test "perfect game" <|
            \_ ->
                let
                    pins =
                        Array.initialize 12 (always 10)
                            |> Debug.log "pins"
                in
                rollPins pins
                    |> score
                    |> Expect.equal 300
        ]


rollPins pins =
    Array.foldl (\pin acc -> roll acc pin) Array.empty pins
