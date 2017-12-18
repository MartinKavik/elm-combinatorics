module Combinatorics.Helpers
    exposing
        ( factorial
        , isValidResult
        )

{-| Helper functions


# Combinatorics computations

@docs factorial


# Other

@docs isValidResult

-}


{-| Calculates the factorial of the given number.

Limitations: n >= 0

    factorial 5 == Ok 120

-}
factorial : Int -> Result String Int
factorial n =
    if n < 0 then
        Err <| "factorial limitation: n >= 0, your n = " ++ toString n
    else
        Ok (factorial_ n)


factorial_ : Int -> Int
factorial_ n =
    case n of
        0 ->
            1

        n ->
            n * factorial_ (n - 1)


{-| Is result valid?

    isValidResult (Err "some error") == True

-}
isValidResult : Result error value -> Bool
isValidResult result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
