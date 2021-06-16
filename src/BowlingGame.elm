{--

 The Bowling Game functions

--}


module BowlingGame exposing (Rolls, roll, score)

import Array exposing (Array)
import Array.Extra as Array


type alias Rolls =
    Array Int


roll : Rolls -> Int -> Rolls
roll rolls pins =
    Array.push pins rolls


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
        sumOfFrame values =
            getOrZero 0 values
                + getOrZero 1 values

        spareBonus values =
            getOrZero 2 values

        strikeBonus values =
            getOrZero 1 values
                + getOrZero 2 values

        scoreThisFrame values =
            case getOrZero 0 values of
                10 ->
                    10 + strikeBonus values

                _ ->
                    case sumOfFrame values of
                        10 ->
                            10 + spareBonus values

                        n ->
                            n

        nextFrame values =
            case getOrZero 0 values of
                10 ->
                    Array.sliceFrom 1 values

                _ ->
                    Array.sliceFrom 2 values

        scoring frame values total =
            case frame of
                10 ->
                    total

                _ ->
                    scoring (frame + 1)
                        (nextFrame values)
                        (total + scoreThisFrame values)
    in
    scoring 0 rolls 0
        |> Debug.log "score"


getOrZero idx values =
    case Array.get idx values of
        Nothing ->
            0

        Just n ->
            n
