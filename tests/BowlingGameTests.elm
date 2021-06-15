module BowlingGameTests exposing (..)

--import Fuzz exposing (Fuzzer, int, list, string)

import Array exposing (Array, foldl, fromList, map, repeat)
import BowlingGame exposing (Rolls, roll, score)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Bowling Game Score"
        [ test "all gutter balls" <|
            \_ ->
                let
                    rolls =
                        repeat 20 0
                in
                foldl (\pin acc -> roll acc pin) Array.empty rolls
                    |> score
                    |> Expect.equal 0
        , test "all ones" <|
            \_ ->
                let
                    rolls =
                        repeat 20 1
                in
                foldl (\pin acc -> roll acc pin) Array.empty rolls
                    |> score
                    |> Expect.equal 20
        , test "one spare" <|
            \_ ->
                let
                    rolls =
                        Array.append
                            (fromList [ 5, 5, 3 ])
                            (repeat 17 0)
                in
                foldl (\pin acc -> roll acc pin) Array.empty rolls
                    |> score
                    |> Expect.equal 16
        , test "one strike" <|
            \_ ->
                let
                    rolls =
                        Array.append
                            (fromList [ 10, 3, 4 ])
                            (repeat 16 0)
                            |> Debug.log "rolls"
                in
                foldl (\pin acc -> roll acc pin) Array.empty rolls
                    |> score
                    |> Expect.equal 24
        , test "perfect game" <|
            \_ ->
                let
                    rolls =
                        Array.initialize 12 (always 10)
                in
                foldl (\pin acc -> roll acc pin) Array.empty rolls
                    |> score
                    |> Expect.equal 300
        ]
