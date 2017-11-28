port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Keyboard
import Notes exposing (..)
import WesternMusicData exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


port audioStart : String -> Cmd msg


port audioStop : String -> Cmd msg


port bpmChanged : String -> Cmd msg


port notesChanged : List String -> Cmd msg



-- MODEL


arpeggioPatterns : List String
arpeggioPatterns =
    [ "Up"
    , "Down"
    , "Up And Down"
    ]


type alias Model =
    { octave : Octave
    , range : OctaveRange
    , formulaName : FormulaName
    , mode : Mode
    , note : Note
    , rootNoteDouble : Bool
    , audioPlaying : Bool
    , arpeggioPatterns : String
    , bpm : Int
    }


init : ( Model, Cmd Msg )
init =
    let
        initialModel =
            { formulaName = "Ionian/Major"
            , mode = scaleMode
            , note = "C"
            , octave = 4
            , range = 3
            , audioPlaying = False
            , bpm = 120
            , rootNoteDouble = False
            , arpeggioPatterns = "Up"
            }
    in
    ( initialModel
    , notesChanged (notesForAudio initialModel)
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = MatrixChanged String String
    | OctaveRangeChanged String
    | ToggleAudio
    | ArpeggioPatternChanged String
    | BPMChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OctaveRangeChanged r ->
            let
                newModel =
                    { model | range = Result.withDefault 1 (String.toInt r) }
            in
            ( newModel
            , notesChanged (notesForAudio newModel)
            )

        ToggleAudio ->
            case model.audioPlaying of
                True ->
                    ( { model | audioPlaying = False }
                    , audioStop "nevermind"
                    )

                False ->
                    ( { model | audioPlaying = True }
                    , audioStart "nevermind"
                    )

        ArpeggioPatternChanged p ->
            let
                newModel =
                    { model | arpeggioPatterns = p }
            in
            ( newModel
            , notesChanged (notesForAudio newModel)
            )

        BPMChanged newBpm ->
            let
                newModel =
                    { model | bpm = Result.withDefault 120 (String.toInt newBpm) }
            in
            ( newModel
            , bpmChanged (newModel.bpm |> toString)
            )

        MatrixChanged m v ->
            case m of
                "Formula" ->
                    let
                        newModel =
                            { model | formulaName = v }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Octave" ->
                    let
                        newModel =
                            { model | octave = Result.withDefault 0 (String.toInt v) }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Note" ->
                    let
                        newModel =
                            { model | note = v }
                    in
                    ( newModel
                    , notesChanged (notesForAudio newModel)
                    )

                "Mode" ->
                    case v of
                        "Scale" ->
                            let
                                newModel =
                                    { model | mode = v, formulaName = ionian }
                            in
                            ( newModel
                            , notesChanged (notesForAudio newModel)
                            )

                        "Chord" ->
                            let
                                newModel =
                                    { model | mode = v, formulaName = major }
                            in
                            ( newModel
                            , notesChanged (notesForAudio newModel)
                            )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- Html Select Helpers


choiceToBool : String -> Bool
choiceToBool s =
    case s of
        "Yes" ->
            True

        "No" ->
            False

        _ ->
            False


boolToChoice : Bool -> String
boolToChoice b =
    case b of
        True ->
            "Yes"

        False ->
            "No"


stringToOption : String -> Html Msg
stringToOption v =
    option [ value v ] [ text v ]


intToOption : OctaveRange -> Int -> Html Msg
intToOption range i =
    option
        [ value (toString i)
        , selected (i == range)
        ]
        [ text (toString i) ]


boolToOption : Bool -> String -> Html Msg
boolToOption dedup choice =
    option
        [ value choice
        , selected (choice == boolToChoice dedup)
        ]
        [ text choice ]



-- Matrices of Div's


cellOfString : String -> String -> String -> Html Msg
cellOfString matrixName thisCellName selectedCellName =
    div
        [ onClick (MatrixChanged matrixName thisCellName)
        , classList
            [ ( "cell", True )
            , ( String.toLower matrixName, True )
            , ( "cell-selected", thisCellName == selectedCellName )
            ]
        ]
        [ text thisCellName ]


matrixOfStrings : String -> List String -> String -> List (Html Msg)
matrixOfStrings matrixName theList theSelectedItem =
    List.map (\s -> cellOfString matrixName s theSelectedItem) theList



-- VIEWS


selectionH1 : Model -> Html Msg
selectionH1 model =
    h1 []
        [ text <|
            model.note
                ++ " "
                ++ toString model.octave
                ++ " - "
                ++ model.formulaName
        ]


modeMatrixDiv : Model -> Html Msg
modeMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "mode", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Mode" [ scaleMode, chordMode ] model.mode)


formulaMatrixDiv : Model -> Html Msg
formulaMatrixDiv model =
    if model.mode == chordMode then
        div
            [ classList
                [ ( "matrix", True )
                , ( "pattern", True )
                , ( "inline-block", True )
                ]
            ]
            (matrixOfStrings "Formula" (formulaNames chordFormulaPool) model.formulaName)
    else
        div
            [ classList
                [ ( "matrix", True )
                , ( "pattern", True )
                , ( "inline-block", True )
                ]
            ]
            (matrixOfStrings "Formula" (formulaNames scaleFormulaPool) model.formulaName)


noteMatrixDiv : Model -> Html Msg
noteMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "note", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Note" noteNames model.note)


octaveMatrixDiv : Model -> Html Msg
octaveMatrixDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "octave", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Octave"
            (List.map (\i -> toString i) (List.range 0 8))
            (toString model.octave)
        )


octaveRangeSelect : Model -> Html Msg
octaveRangeSelect model =
    div
        [ class "select-cell" ]
        [ select [ onInput OctaveRangeChanged, name "Range", class "soflow" ]
            -- pass the model range as first argument so the menu can display '3' on start up
            (List.range 1 6 |> List.map (intToOption model.range))
        ]


notesForAudio : Model -> List String
notesForAudio model =
    let
        notes =
            notesForModelState model
    in
    case model.arpeggioPatterns of
        "Up" ->
            notes

        "Down" ->
            List.reverse notes

        "Up And Down" ->
            -- List.reverse notes |> List.append notes
            notes ++ List.reverse notes

        _ ->
            notes


notesForModelState : Model -> List String
notesForModelState model =
    if model.mode == chordMode then
        chord model.note
            model.octave
            model.formulaName
            model.range
            (not model.rootNoteDouble)
            0
            |> List.take 36
            |> List.map (\i -> midiNoteNumberToString i)
    else
        scale model.note
            model.octave
            model.formulaName
            model.range
            (not model.rootNoteDouble)
            |> List.take 36
            |> List.map (\i -> midiNoteNumberToString i)


resultMatrixDiv : Model -> Html Msg
resultMatrixDiv model =
    let
        selectedCellNONE =
            ""
    in
    div
        [ classList
            [ ( "matrix", True )
            , ( "result", True )
            , ( "inline-block", True )
            ]
        ]
        (matrixOfStrings "Result"
            (notesForModelState model)
            selectedCellNONE
        )


audioBannerDiv : Model -> Html Msg
audioBannerDiv model =
    div [ class "select-cell" ]
        [ div
            [ id "audio-banner"
            ]
            [ if model.audioPlaying == False then
                button [ onClick ToggleAudio ] [ text "Play" ]
              else
                button [ onClick ToggleAudio ] [ text "Stop" ]
            , input
                [ onInput BPMChanged
                , id "bpm"
                , type_ "range"
                , Html.Attributes.min "10"
                , Html.Attributes.max "200"
                , Html.Attributes.value (model.bpm |> toString)
                ]
                []
            , span [] [ text (model.bpm |> toString) ]
            ]
        ]


arpeggioPatternsSelect : Model -> Html Msg
arpeggioPatternsSelect model =
    div [ class "select-cell" ]
        [ select [ onInput ArpeggioPatternChanged, name "Pattern", class "soflow" ]
            (arpeggioPatterns |> List.map stringToOption)
        ]


keyboardSelectionDiv : Model -> Html Msg
keyboardSelectionDiv model =
    div [ class "select-wrapper" ]
        [ div [ class "select-row" ]
            [ div [ class "select-cell-caption" ] [ text "Octave Range :" ]
            , octaveRangeSelect model
            , div [ class "select-cell-caption" ] [ text "Pattern :" ]
            , arpeggioPatternsSelect model
            , div [ class "select-cell-caption" ] [ text "Control: " ]
            , audioBannerDiv model
            ]
        ]


keyboardDiv : Model -> Html Msg
keyboardDiv model =
    div
        [ classList
            [ ( "matrix", True )
            , ( "result", True )
            , ( "inline-block", False )
            ]
        ]
        [ Keyboard.renderBoard
            { mode = model.mode
            , formulaName = model.formulaName
            , note = model.note
            , range = model.range
            , octave = model.octave
            }
        ]


view : Model -> Html Msg
view model =
    div [ class "centered-wrapper" ]
        [ modeMatrixDiv model
        , selectionH1 model
        , noteMatrixDiv model
        , octaveMatrixDiv model
        , formulaMatrixDiv model
        , resultMatrixDiv model
        , keyboardSelectionDiv model
        , keyboardDiv model
        ]
