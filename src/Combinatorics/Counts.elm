module Combinatorics.Counts
    exposing
        ( countCombinations
        , countCombinationsWithReps
        , countPermutations
        , countPermutationsWithReps
        , countVariations
        , countVariationsWithReps
        )

{-| **count* functions** - calculate the number of all possible comb. / permut. / variat.

All functions return `Ok Int` or `Err String`.

Error example: `"countVariations limitation: k >= 0, your k = -2"`

_All functions are mirrored mathemetical formulas, create issue / pull requests for more effective computations._


# Variations

@docs countVariationsWithReps, countVariations


# Permutations

@docs countPermutationsWithReps, countPermutations


# Combinations

@docs countCombinationsWithReps, countCombinations

-}

import Combinatorics.Helpers exposing (factorial)


fact : Int -> Int
fact n =
    Result.withDefault 1 (factorial n)


{-| Calculates the number of variations with repetitions.

Limitations: k >= 0, n >= 0

Formula: V'(k,n) = n ^ k

    countVariationsWithReps 2 3 == Ok 9

-}
countVariationsWithReps : Int -> Int -> Result String Int
countVariationsWithReps k n =
    if k < 0 then
        Err <| "countVariationsWithReps limitation: k >= 0, your k = " ++ toString k
    else if n < 0 then
        Err <| "countVariationsWithReps limitation: n >= 0, your n = " ++ toString n
    else
        Ok (countVariationsWithReps_ k n)


countVariationsWithReps_ : Int -> Int -> Int
countVariationsWithReps_ k n =
    n ^ k


{-| Calculates the number of variations.

Limitations: k >= 0, n >= 0, n >= k

Formula: V'(k,n) = n! / (n-k)!

    countVariations 2 3 == Ok 6

-}
countVariations : Int -> Int -> Result String Int
countVariations k n =
    if k < 0 then
        Err <| "countVariations limitation: k >= 0, your k = " ++ toString k
    else if n < 0 then
        Err <| "countVariations limitation: n >= 0, your n = " ++ toString n
    else if n < k then
        Err <|
            "countVariations limitation: n >= k, your n = "
                ++ toString n
                ++ " and your k = "
                ++ toString k
    else
        Ok (countVariations_ k n)


countVariations_ : Int -> Int -> Int
countVariations_ k n =
    fact n // fact (n - k)


{-| Calculates the number of permutations with repetitions.

Limitations: kx >= 0

Formula: P'(k1,k2,...,kn) = (k1+k2+,...,+kn)! / k1! . k2! . ,..., . kn!

    countPermutationsWithReps [ 2, 2 ] == Ok 6

-}
countPermutationsWithReps : List Int -> Result String Int
countPermutationsWithReps ks =
    if List.any ((>) 0) ks then
        Err <| "countPermutationsWithReps limitation: kx >= 0, your ks = " ++ toString ks
    else
        Ok (countPermutationsWithReps_ ks)


countPermutationsWithReps_ : List Int -> Int
countPermutationsWithReps_ ks =
    fact (List.sum ks) // (List.product <| List.map fact ks)


{-| Calculates the number of permutations.

Limitations: n >= 0

Formula: P(n) = n!

    countPermutations 3 == Ok 6

-}
countPermutations : Int -> Result String Int
countPermutations n =
    if n < 0 then
        Err <| "countPermutations limitation: n >= 0, your n = " ++ toString n
    else
        Ok (countPermutations_ n)


countPermutations_ : Int -> Int
countPermutations_ n =
    fact n


{-| Calculates the number of combinations with repetitions.

Limitations: k >= 0, n > 0

Formula: C'(k,n) = (n+k-1)! / k!(n-1)!

    countCombinationsWithReps 2 4 == Ok 10

-}
countCombinationsWithReps : Int -> Int -> Result String Int
countCombinationsWithReps k n =
    if k < 0 then
        Err <| "countCombinationsWithReps limitation: k >= 0, your k = " ++ toString k
    else if n <= 0 then
        Err <| "countCombinationsWithReps limitation: n >= 0, your n = " ++ toString n
    else
        Ok (countCombinationsWithReps_ k n)


countCombinationsWithReps_ : Int -> Int -> Int
countCombinationsWithReps_ k n =
    fact (n + k - 1) // (fact k * fact (n - 1))


{-| Calculates the number of combinations.

Limitations: k >= 0, n >= 0, n >= k

Formula: C(k,n) = n! / k!(n-k)!

    countCombinations 2 4 == Ok 6

-}
countCombinations : Int -> Int -> Result String Int
countCombinations k n =
    if k < 0 then
        Err <| "countCombinations limitation: k >= 0, your k = " ++ toString k
    else if n < 0 then
        Err <| "countCombinations limitation: n >= 0, your n = " ++ toString n
    else if n < k then
        Err <|
            "countCombinations limitation: n >= k, your n = "
                ++ toString n
                ++ " and your k = "
                ++ toString k
    else
        Ok (countCombinations_ k n)


countCombinations_ : Int -> Int -> Int
countCombinations_ k n =
    fact n // (fact k * fact (n - k))
