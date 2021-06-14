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

        scoring values total =
            let
                val =
                    get 0 values
                        |> Debug.log "scoring"
            in
            case val of
                Nothing ->
                    total

                Just 10 ->
                    let
                        b1 =
                            getOrZero 1 values
                                |> Debug.log "first ball"

                        b2 =
                            getOrZero 2 values
                                |> Debug.log "second ball"

                        newTotal =
                            total
                                + 10
                                + b1
                                + b2
                                |> Debug.log "new total"
                    in
                    scoring (slice 1 (length values) values) newTotal
                        |> Debug.log "strike"

                Just n ->
                    let
                        frame =
                            n + getOrZero 1 values
                    in
                    case frame of
                        10 ->
                            scoring (slice 2 (length values) values) total
                                + frame
                                + getOrZero 2 values
                                |> Debug.log "spare"

                        _ ->
                            scoring (slice 2 (length values) values) total
                                + frame
                                |> Debug.log "frame"
    in
    scoring rolls 0
        |> Debug.log "score"
