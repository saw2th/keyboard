module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Keyboard exposing (..)
import KeyboardConstants exposing (..)
import Notes exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "The Keyboard module"
        [ describe "offsetFromCminus1"
            [ test "outputs 9" <|
                \_ ->
                    Expect.equal 21
                        (offsetFromCminus1
                            kbStandardPiano.firstNote
                            kbStandardPiano.firstOctave
                        )
            , test "lowestMidiNote" <|
                \_ ->
                    Expect.equal 21
                        (Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber kbStandardPiano.firstNote kbStandardPiano.firstOctave))
            ]
        ]
