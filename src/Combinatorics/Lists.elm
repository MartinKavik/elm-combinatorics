module Combinatorics.Lists
    exposing
        ( getCombinations
        , getCombinationsWithReps
        , getPermutations
        , getPermutationsWithReps
        , getVariations
        , getVariationsWithReps
        )

{-| **get* functions** - generate the list of all possible comb. / permut. / variat.

All functions return `Ok list` or `Err error`.

Error example: `"getVariations limitation: k >= 0, your k = -2"`

_Create issue / pull requests for more effective computations, lazy loadings, etc._


# Variations

@docs getVariationsWithReps, getVariations


# Permutations

@docs getPermutationsWithReps, getPermutations


# Combinations

@docs getCombinationsWithReps, getCombinations

-}

import List.Extra exposing (allDifferentBy)


{-| Get list of variations with repetitions.

Limitations: k >= 0, n >= 0

    getVariationsWithReps 2 [ "a", "b" ] == Ok [["a","a"],["a","b"],["b","a"],["b","b"]]

-}
getVariationsWithReps : Int -> List a -> Result String (List (List a))
getVariationsWithReps k set =
    if k < 0 then
        Err <| "getVariationsWithReps limitation: k >= 0, your k = " ++ toString k
    else
        Ok (getVariationsWithReps_ k set)


getVariationsWithReps_ : Int -> List a -> List (List a)
getVariationsWithReps_ k set =
    let
        doGetVariationsWithReps k set depth resultItem =
            if depth < k then
                set
                    |> List.concatMap
                        (\setItem -> doGetVariationsWithReps k set (depth + 1) (setItem :: resultItem))
            else
                [ resultItem |> List.reverse ]
    in
    doGetVariationsWithReps k set 0 []


{-| Get list of variations.

Limitations: k >= 0, (List.length set) >= k

    getVariations 2 [ "a", "b" ] == Ok [["a","b"],["b","a"]]

-}
getVariations : Int -> List a -> Result String (List (List a))
getVariations k set =
    if k < 0 then
        Err <| "getVariations limitation: k >= 0, your k = " ++ toString k
    else if List.length set < k then
        Err <|
            "getVariations limitation: (List.length set) >= k, your set = "
                ++ toString set
                ++ " and your k = "
                ++ toString k
    else
        Ok (getVariations_ k set)


getVariations_ : Int -> List a -> List (List a)
getVariations_ k set =
    let
        doGetVariations k set depth resultItem =
            if depth < k then
                set
                    |> List.concatMap
                        (\setItem ->
                            if List.member setItem resultItem then
                                []
                            else
                                doGetVariations k set (depth + 1) (setItem :: resultItem)
                        )
            else
                [ resultItem |> List.reverse ]
    in
    doGetVariations k set 0 []


{-| Get list of permutations with repetitions.

Limitations: kx >= 0 (kx is 2nd value in tuple), a is unique (a is 1st value in tuple)

    getPermutationsWithReps [ ( "a", 1 ), ( "b", 2 ) ] == Ok [["a","b","b"],["b","a","b"],["b","b","a"]]

-}
getPermutationsWithReps : List ( comparable, Int ) -> Result String (List (List comparable))
getPermutationsWithReps setWithCounts =
    if List.any ((>) 0) (setWithCounts |> List.unzip |> Tuple.second) then
        Err <| "getPermutationsWithReps limitation: k >= 0, your [(a,k)] = " ++ toString setWithCounts
    else if not (setWithCounts |> allDifferentBy Tuple.first) then
        Err <| "getPermutationsWithReps limitation: a is unique, your [(a,k)] = " ++ toString setWithCounts
    else
        Ok (getPermutationsWithReps_ setWithCounts)


getPermutationsWithReps_ : List ( a, Int ) -> List (List a)
getPermutationsWithReps_ setWithCounts =
    let
        totalCount =
            setWithCounts |> List.unzip |> Tuple.second |> List.sum

        doGetPermutationsWithReps k set depth resultItem =
            if depth < k then
                set
                    |> List.concatMap
                        (\( setItem, count ) ->
                            if (resultItem |> List.filter ((==) setItem) |> List.length) == count then
                                []
                            else
                                doGetPermutationsWithReps k set (depth + 1) (setItem :: resultItem)
                        )
            else
                [ resultItem |> List.reverse ]
    in
    doGetPermutationsWithReps totalCount setWithCounts 0 []


{-| Get list of permutations.

    getPermutations [ "a", "b" ] == Ok [["a","b"],["b","a"]]

-}
getPermutations : List a -> Result String (List (List a))
getPermutations n =
    Ok (getPermutations_ n)


getPermutations_ : List a -> List (List a)
getPermutations_ set =
    getVariations_ (List.length set) set


{-| Get list of combinations with repetitions.

Limitations: k >= 0, (List.length set) > 0

    getCombinationsWithReps 2 [ "a", "b" ] == Ok [["a","a"],["a","b"],["b","b"]]

-}
getCombinationsWithReps : Int -> List a -> Result String (List (List a))
getCombinationsWithReps k set =
    if k < 0 then
        Err <| "getCombinationsWithReps limitation: k >= 0, your k = " ++ toString k
    else if List.length set == 0 then
        Err <| "getCombinationsWithReps limitation: (List.length set) > 0, your set = " ++ toString set
    else
        Ok (getCombinationsWithReps_ k set)


getCombinationsWithReps_ : Int -> List a -> List (List a)
getCombinationsWithReps_ k set =
    if k == 0 then
        [ [] ]
    else
        case set of
            [] ->
                []

            x :: xs ->
                List.map ((::) x) (getCombinationsWithReps_ (k - 1) set) ++ getCombinationsWithReps_ k xs


{-| Get list number of combinations.

Limitations: k >= 0, n >= 0, (List.length set) >= k

    getCombinations 2 [ "a", "b", "c" ] == Ok [["a","b"],["a","c"],["b","c"]]

-}
getCombinations : Int -> List a -> Result String (List (List a))
getCombinations k set =
    if k < 0 then
        Err <| "getCombinations limitation: k >= 0, your k = " ++ toString k
    else if List.length set < k then
        Err <|
            "getCombinations limitation: (List.length set) >= k, your set = "
                ++ toString set
                ++ " and your k = "
                ++ toString k
    else
        Ok (getCombinations_ k set)


getCombinations_ : Int -> List a -> List (List a)
getCombinations_ k set =
    if k == 0 then
        [ [] ]
    else
        case set of
            [] ->
                []

            x :: xs ->
                List.map ((::) x) (getCombinations_ (k - 1) xs) ++ getCombinations_ k xs
