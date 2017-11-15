module KeyboardConstants exposing (..)

import Notes exposing (Note, Octave)


type alias Keyboard =
    { keys : Int
    , firstNote : Note
    , firstOctave : Octave
    }


kbStandardPiano =
    Keyboard 88 "A" 0


kbWhiteWidth =
    80


kbBlackWidth =
    40


kbHalfBlackWidth =
    kbBlackWidth // 2


kbWhiteHeight =
    400


kbBlackHeight =
    280


kbKeyPressRadius =
    15


kbWhiteKeyColour =
    "#FFFFF7"


kbWhiteStrokeColour =
    "#555555"


kbBlackKeyColour =
    "#4B4B4B"


kbBlackStrokeColour =
    "#979797"


kbKeyPressColour =
    "#FF0000"
