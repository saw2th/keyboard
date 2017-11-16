module KeyboardConstants exposing (..)

import Notes exposing (Note, Octave)


type alias Keyboard =
    { keys : Int
    , firstNote : Note
    , firstOctave : Octave
    }


kbStandardPiano : Keyboard
kbStandardPiano =
    Keyboard 88 "A" 0


kbWhiteWidth : Int
kbWhiteWidth =
    80


kbBlackWidth : Int
kbBlackWidth =
    40


kbHalfBlackWidth : Int
kbHalfBlackWidth =
    kbBlackWidth // 2


kbWhiteHeight : Int
kbWhiteHeight =
    400


kbBlackHeight : Int
kbBlackHeight =
    280


kbKeyPressRadius : Int
kbKeyPressRadius =
    15


kbWhiteKeyColour : String
kbWhiteKeyColour =
    "#FFFFF7"


kbWhiteStrokeColour : String
kbWhiteStrokeColour =
    "#555555"


kbBlackKeyColour : String
kbBlackKeyColour =
    "#4B4B4B"


kbBlackStrokeColour : String
kbBlackStrokeColour =
    "#979797"


kbKeyPressColour : String
kbKeyPressColour =
    "#FF0000"
