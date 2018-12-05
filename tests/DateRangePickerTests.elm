module DateRangePickerTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import DateRangePicker exposing (..)


suite : Test
suite =
    describe "DateRangePicker"
        [ describe "init"
            [ test "dates are Nothing by default" <|
                \_ ->
                    let
                        (picker, _, _) = DateRangePicker.init 
                    in
                        Expect.all
                            [ \p -> p.startDate |> Expect.equal Nothing
                            , \p -> p.endDate |> Expect.equal Nothing
                            ]
                            picker
            ]
        ]
