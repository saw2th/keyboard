module Keyboard exposing (..)

import Array
import Html.Attributes
import KeyboardConstants exposing (..)
import Notes exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import WesternMusicData exposing (..)


type KeyboardTypes
    = List Keyboard


type alias NoteSpec =
    { formulaName : FormulaName
    , note : Note
    , range : OctaveRange
    , octave : Octave
    , mode : Mode
    }


type alias KeyParams =
    { x : Int
    , y : Int
    }


type alias KeyPressCoord =
    { x : Int
    , y : Int
    }


type alias KeySpec =
    { black : Int
    , width : Int
    }


renderBoard : NoteSpec -> Svg msg
renderBoard noteSpec =
    let
        renderedKeyboard =
            if noteSpec.mode == scaleMode then
                Notes.scale noteSpec.note noteSpec.octave noteSpec.formulaName noteSpec.range False |> renderKeys
            else
                Notes.chord noteSpec.note noteSpec.octave noteSpec.formulaName noteSpec.range False 0 |> renderKeys
    in
    svg
        [ width "830"
        , height "80"
        , viewBox "0 0 4480 400"
        , preserveAspectRatio "xMidYMid"
        , fill "white"
        , stroke "black"
        , strokeWidth "3"
        , Html.Attributes.style [ ( "margin-left", "75px" ) ]
        ]
        renderedKeyboard



{--offset is how far first note is from C-1
still some fiddle factors for discrepancy between C4 midi and C4 piano
--}


board : Int -> Int -> Int -> Int -> List KeySpec
board offset numberKeys key whiteCount =
    if numberKeys == 0 then
        []
    else
        let
            whiteKey =
                isNoteWhite ((key + offset) % 12)

            offsetBoard =
                board offset
        in
        if whiteKey then
            [ { width = whiteCount * kbWhiteWidth, black = 0 } ] ++ offsetBoard (numberKeys - 1) (key + 1) (whiteCount + 1)
        else
            [ { width = whiteCount * kbWhiteWidth - kbHalfBlackWidth, black = 1 } ] ++ offsetBoard (numberKeys - 1) (key + 1) whiteCount


offsetFromCminus1 : Note -> Octave -> Int
offsetFromCminus1 lowestNote lowestOcatve =
    Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber lowestNote lowestOcatve)


renderKeys : NoteCollection -> List (Svg msg)
renderKeys noteCollection =
    let
        offset =
            offsetFromCminus1 kbStandardPiano.firstNote kbStandardPiano.firstOctave

        mixedKeys =
            board offset kbStandardPiano.keys 0 0
    in
    List.map renderKey (List.sortBy .black mixedKeys) ++ List.map renderKeyPress noteCollection


renderKey : KeySpec -> Svg msg
renderKey keySpec =
    if .black keySpec == 0 then
        .width keySpec |> renderWhiteKey
    else
        .width keySpec |> renderBlackKey


renderWhiteKey : Int -> Svg msg
renderWhiteKey xVal =
    rect
        [ class "piano-key white-key"
        , stroke kbWhiteStrokeColour
        , fill kbWhiteKeyColour
        , x (xVal |> toString)
        , y "0"
        , width (kbWhiteWidth |> toString)
        , height (kbWhiteHeight |> toString)
        ]
        []


renderBlackKey : Int -> Svg msg
renderBlackKey xVal =
    rect
        [ class "piano-key black-key"
        , stroke kbBlackStrokeColour
        , fill kbBlackKeyColour
        , x (xVal |> toString)
        , y "0"
        , width (kbBlackWidth |> toString)
        , height (kbBlackHeight |> toString)
        ]
        []


renderKeyPress : Int -> Svg msg
renderKeyPress midiNote =
    let
        offset =
            offsetFromCminus1 kbStandardPiano.firstNote kbStandardPiano.firstOctave

        keyPressCoord =
            keyOffset midiNote offset

        lowestMidiNote =
            Maybe.withDefault 0 (noteAndOctaveToMidiNoteNumber kbStandardPiano.firstNote kbStandardPiano.firstOctave)

        highestMidiNote =
            lowestMidiNote + kbStandardPiano.keys
    in
    if midiNote >= lowestMidiNote && midiNote < highestMidiNote then
        circle
            [ class "piano-keypress"
            , stroke kbBlackStrokeColour
            , fill kbKeyPressColour
            , cx (keyPressCoord.x |> toString)
            , cy (keyPressCoord.y |> toString)
            , r (kbKeyPressRadius |> toString)
            ]
            []
    else
        circle [] []



{--Pattern of black and white keys starting on C
--}


keyPattern : List Bool
keyPattern =
    [ True, False, True, False, True, True, False, True, False, True, False, True ]


keyPatternArray : Array.Array Bool
keyPatternArray =
    Array.fromList keyPattern


keyWidthFromKeyColour : Bool -> Int
keyWidthFromKeyColour whiteNote =
    if whiteNote then
        kbWhiteWidth
    else
        0


positionInChromaticeScale : Int -> Int
positionInChromaticeScale midiNote =
    midiNote % 12


octaveAboveCminus1 : Int -> Int
octaveAboveCminus1 midiNote =
    midiNote // 12


widthFromCminus1 : Int -> Int
widthFromCminus1 midiNote =
    let
        lastOctaveWhite =
            List.map keyWidthFromKeyColour (List.take (positionInChromaticeScale midiNote) keyPattern)
                |> List.sum
    in
    octaveAboveCminus1 midiNote * 7 * kbWhiteWidth + lastOctaveWhite


keyOffset : Int -> Int -> KeyPressCoord
keyOffset midiNote offset =
    let
        whiteNote =
            positionInChromaticeScale midiNote |> isNoteWhite

        xoffset =
            widthFromCminus1 midiNote
                - widthFromCminus1 offset
                + (kbWhiteWidth
                    // 2
                    - (if whiteNote then
                        0
                       else
                        kbBlackWidth
                      )
                  )

        yoffset =
            if whiteNote then
                toFloat kbWhiteHeight / 1.2 |> round
            else
                toFloat kbBlackHeight / 1.2 |> round
    in
    { x = xoffset, y = yoffset }


isNoteWhite : Int -> Bool
isNoteWhite index =
    let
        whiteBlackBoolMaybe =
            Array.get index keyPatternArray
    in
    Maybe.withDefault False whiteBlackBoolMaybe
