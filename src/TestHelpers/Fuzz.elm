module TestHelpers.Fuzz
    exposing
        ( listOfLength
        )

import Fuzz exposing (Fuzzer)


listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength listLen fuzzer =
    List.foldl
        (Fuzz.map2 (\elem -> \list -> elem :: list))
        (Fuzz.constant [])
        (List.repeat listLen fuzzer)
