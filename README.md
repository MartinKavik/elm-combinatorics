## Combinations, Permutations and Variations in Elm

Modules and their functions:

**Combinatorics.Counts**
```elm
countVariationsWithReps 2 3 == Ok 9

countVariations 2 3 == Ok 6

countPermutationsWithReps [ 2, 2 ] == Ok 6

countPermutations 3 == Ok 6

countCombinationsWithReps 2 4 == Ok 10

countCombinations 2 4 == Ok 6
```

**Combinatorics.Lists**
```elm
getVariationsWithReps 2 [ "a", "b" ] == Ok [["a","a"],["a","b"],["b","a"],["b","b"]]

getVariations 2 [ "a", "b" ] == Ok [["a","b"],["b","a"]]

getPermutationsWithReps [ ( "a", 1 ), ( "b", 2 ) ] == Ok [["a","b","b"],["b","a","b"],["b","b","a"]]

getPermutations [ "a", "b" ] == Ok [["a","b"],["b","a"]]

getCombinationsWithReps 2 [ "a", "b" ] == Ok [["a","a"],["a","b"],["b","b"]]

getCombinations 2 [ "a", "b", "c" ] == Ok [["a","b"],["a","c"],["b","c"]]
```

**Combinatorics.Helpers**
```elm
factorial 5 == Ok 120

isValidResult (Err "some error") == False
```