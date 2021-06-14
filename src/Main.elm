module Main exposing (Msg(..), main, update, view)

import Browser
import Game exposing (roll, score)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



{--
 Bowling Game scoring kata.

 Game consists of 10 frames, 2 rolls per frame except last frame has up to 3 rolls.
 Scoring each frame total of pins knocked down
 If spare, 10 pins down in 2 rolls, bonus is number of pins in next roll
 If strike, 10 pins down in 1 roll, bonux is number of pins in next 2 rolls
 Tenth frame, if spare or strike, roll a third time.
 Perfect game scores 300 = (10+20)*10

 Functions

 roll (int): int is number of pins knocked down, 0 to 10
 score: calculates the score

--}


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
