{--

 The Bowling Game functions

--}


module BowlingGame exposing (Rolls, roll, score)

import Array exposing (Array, foldl, get, length, push, slice, toList)


type alias Rolls =
    Array Int


roll : Rolls -> Int -> Rolls
roll rolls pins =
    push pins rolls


score : Rolls -> Int
score rolls =
    {-
       Scoring 10 frames
       1 or 2 rolls per frame
       last frame has 2 or 3 rolls
       Spare = 10 + next roll
       Strike = 10 + next 2 rolls
    -}
    let
        getOrZero idx values =
            let
                res =
                    get idx values
            in
            case res of
                Nothing ->
                    0

                Just n ->
                    n

        scoring frame values total =
            case ( frame, get 0 values ) |> Debug.log "scoring" of
                ( 10, _ ) ->
                    total

                ( _, Nothing ) ->
                    total

                ( frameIndex, Just 10 ) ->
                    let
                        b1 =
                            getOrZero 1 values

                        b2 =
                            getOrZero 2 values

                        newTotal =
                            total
                                + 10
                                + b1
                                + b2
                    in
                    scoring (frameIndex + 1)
                        (slice 1 (length values) values)
                        newTotal

                ( frameIndex, Just n ) ->
                    let
                        frameTotal =
                            n + getOrZero 1 values
                    in
                    case frameTotal of
                        10 ->
                            scoring (frameIndex + 1)
                                (slice 2 (length values) values)
                                total
                                + frameTotal
                                + getOrZero 2 values

                        _ ->
                            scoring (frameIndex + 1)
                                (slice 2 (length values) values)
                                total
                                + frameTotal
    in
    scoring 0 rolls 0
        |> Debug.log "score"
