module CombinatoricsTest exposing (..)

import Combinatorics.Counts exposing (..)
import Combinatorics.Helpers exposing (factorial, isValidResult)
import Combinatorics.Lists exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (char, intRange, list, tuple)
import List.Extra exposing (uniqueBy)
import Test exposing (..)
import TestHelpers.Fuzz exposing (listOfLength)


helpers : Test
helpers =
    describe "Test helper"
        [ test "factorial" <|
            \_ ->
                Ok 120 |> Expect.equal (factorial 5)
        , test "factorial fail: n < 0" <|
            \_ ->
                Err "factorial limitation: n >= 0, your n = -5"
                    |> Expect.equal (factorial -5)
        , test "isValidResult" <|
            \_ ->
                isValidResult (Err "some error")
                    |> Expect.false "isValidResult should be false"
        ]


getCounts : Test
getCounts =
    describe "Get count of"
        [ test "variations with repetitions" <|
            \_ ->
                Ok 9 |> Expect.equal (countVariationsWithReps 2 3)

        ----------
        , test "variations" <|
            \_ ->
                Ok 6 |> Expect.equal (countVariations 2 3)

        ----------
        , test "permutations with repetitions" <|
            \_ ->
                Ok 6 |> Expect.equal (countPermutationsWithReps [ 2, 2 ])

        ----------
        , test "permutations" <|
            \_ ->
                Ok 6 |> Expect.equal (countPermutations 3)

        ----------
        , test "combinations with repetitions" <|
            \_ ->
                Ok 10 |> Expect.equal (countCombinationsWithReps 2 4)

        ----------
        , test "combinations" <|
            \_ ->
                Ok 6 |> Expect.equal (countCombinations 2 4)
        ]


getLists : Test
getLists =
    describe "Get list of"
        [ test "variations with repetitions" <|
            \_ ->
                Ok
                    [ [ "a", "a" ]
                    , [ "a", "b" ]
                    , [ "a", "c" ]
                    , [ "b", "a" ]
                    , [ "b", "b" ]
                    , [ "b", "c" ]
                    , [ "c", "a" ]
                    , [ "c", "b" ]
                    , [ "c", "c" ]
                    ]
                    |> Expect.equal (getVariationsWithReps 2 [ "a", "b", "c" ])

        ----------
        , test "variations" <|
            \_ ->
                Ok
                    [ [ "a", "b" ]
                    , [ "a", "c" ]
                    , [ "b", "a" ]
                    , [ "b", "c" ]
                    , [ "c", "a" ]
                    , [ "c", "b" ]
                    ]
                    |> Expect.equal (getVariations 2 [ "a", "b", "c" ])

        ----------
        , test "permutations with repetitions" <|
            \_ ->
                Ok
                    [ [ "a", "a", "b", "b" ]
                    , [ "a", "b", "a", "b" ]
                    , [ "a", "b", "b", "a" ]
                    , [ "b", "a", "a", "b" ]
                    , [ "b", "a", "b", "a" ]
                    , [ "b", "b", "a", "a" ]
                    ]
                    |> Expect.equal (getPermutationsWithReps [ ( "a", 2 ), ( "b", 2 ) ])

        ----------
        , test "permutations" <|
            \_ ->
                Ok
                    [ [ "a", "b", "c" ]
                    , [ "a", "c", "b" ]
                    , [ "b", "a", "c" ]
                    , [ "b", "c", "a" ]
                    , [ "c", "a", "b" ]
                    , [ "c", "b", "a" ]
                    ]
                    |> Expect.equal (getPermutations [ "a", "b", "c" ])

        ----------
        , test "combinations with repetitions" <|
            \_ ->
                Ok
                    [ [ "a", "a" ]
                    , [ "a", "b" ]
                    , [ "a", "c" ]
                    , [ "a", "d" ]
                    , [ "b", "b" ]
                    , [ "b", "c" ]
                    , [ "b", "d" ]
                    , [ "c", "c" ]
                    , [ "c", "d" ]
                    , [ "d", "d" ]
                    ]
                    |> Expect.equal (getCombinationsWithReps 2 [ "a", "b", "c", "d" ])

        ----------
        , test "combinations" <|
            \_ ->
                Ok
                    [ [ "a", "b" ]
                    , [ "a", "c" ]
                    , [ "a", "d" ]
                    , [ "b", "c" ]
                    , [ "b", "d" ]
                    , [ "c", "d" ]
                    ]
                    |> Expect.equal (getCombinations 2 [ "a", "b", "c", "d" ])
        ]


compareListsAndCounts : Test
compareListsAndCounts =
    describe "Compare list length and count of"
        [ fuzz2 (intRange 0 8) (intRange 0 5) "variations with repetitions" <|
            \k n ->
                let
                    set =
                        List.range 1 n
                in
                List.length (getVariationsWithReps k set |> Result.withDefault [])
                    |> Expect.equal (countVariationsWithReps k n |> Result.withDefault -1)

        ----------
        , fuzz2 (intRange 0 5) (intRange 0 5) "variations" <|
            \n k ->
                let
                    set =
                        List.range 1 n

                    variationsList =
                        getVariations k set

                    variationsCount =
                        countVariations k n
                in
                if k > n then
                    [ isValidResult variationsList, isValidResult variationsCount ]
                        |> Expect.equalLists [ False, False ]
                else
                    List.length (variationsList |> Result.withDefault [])
                        |> Expect.equal (variationsCount |> Result.withDefault -1)

        ----------
        , fuzz
            (( char, intRange 0 3 )
                |> tuple
                |> listOfLength 3
             -- listRange 0 3 would be better
            )
            "permutations with repetitions"
          <|
            \setWithCounts ->
                -- setWithCounts = e.g. [("a",2), ("x",5)]
                let
                    filteredSet =
                        setWithCounts |> uniqueBy Tuple.first

                    ( _, ks ) =
                        List.unzip filteredSet
                in
                List.length (getPermutationsWithReps filteredSet |> Result.withDefault [])
                    |> Expect.equal (countPermutationsWithReps ks |> Result.withDefault -1)

        ----------
        , fuzz (intRange 0 5) "permutations" <|
            \n ->
                let
                    set =
                        List.range 1 n
                in
                List.length (getPermutations set |> Result.withDefault [])
                    |> Expect.equal (countPermutations n |> Result.withDefault -1)

        ----------
        , fuzz2 (intRange 0 8) (intRange 1 5) "combinations with repetitions" <|
            \k n ->
                let
                    set =
                        List.range 1 n
                in
                List.length (getCombinationsWithReps k set |> Result.withDefault [])
                    |> Expect.equal (countCombinationsWithReps k n |> Result.withDefault -1)

        ----------
        , fuzz2 (intRange 0 3) (intRange 0 5) "combinations" <|
            \k n ->
                let
                    set =
                        List.range 1 n

                    combinationsList =
                        getCombinations k set

                    combinationsCount =
                        countCombinations k n
                in
                if k > n then
                    [ isValidResult combinationsList, isValidResult combinationsCount ]
                        |> Expect.equalLists [ False, False ]
                else
                    List.length (combinationsList |> Result.withDefault [])
                        |> Expect.equal (combinationsCount |> Result.withDefault -1)
        ]
