module Main exposing (main)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import List
import List.Extra
import Markdown
import Platform
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Task


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)


type alias Model =
    { cells : Array.Array Cell
    }


type Content
    = Text
    | Code


type alias Cell =
    { content : Content
    , cursorPosition : SrcLoc
    , isActive : Bool
    , lines : List String
    , raw : String
    }


type alias SrcLoc =
    { column : Int, line : Int }


textCell : String -> Bool -> Cell
textCell raw isActive =
    { content = Text
    , cursorPosition = { column = 1, line = 1 }
    , isActive = isActive
    , lines = String.split "\n" raw
    , raw = raw
    }


codeCell : String -> Bool -> Cell
codeCell raw isActive =
    { content = Code
    , cursorPosition = { column = 1, line = 1 }
    , isActive = isActive
    , lines = String.split "\n" raw
    , raw = raw
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { cells = Array.push (textCell "# Title\n" True) Array.empty
      }
    , Cmd.none
    )


type Msg
    = CellUpdated Int String
    | CellActive Int
    | KeyPressed Key
    | CellFocusOk Int
    | CellFocusErr Int


type Key
    = Escape
    | Unsupported
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | ArrowUp


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey keyCode =
    case keyCode of
        "Escape" ->
            Escape

        "ArrowDown" ->
            ArrowDown

        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        "ArrowUp" ->
            ArrowUp

        _ ->
            Unsupported


updateColumn : Int -> SrcLoc -> SrcLoc
updateColumn inc srcLoc =
    if srcLoc.column + inc < 0 then
        srcLoc

    else
        { srcLoc | column = srcLoc.column + inc }


updateLine : Int -> SrcLoc -> SrcLoc
updateLine inc srcLoc =
    if srcLoc.line + inc < 0 then
        srcLoc

    else
        { srcLoc | line = srcLoc.line + inc }


findActiveCell : Array.Array Cell -> Maybe ( Int, Cell )
findActiveCell =
    let
        isActive : ( Int, Cell ) -> Bool
        isActive ( _, cell ) =
            cell.isActive
    in
    List.Extra.find isActive << Array.toIndexedList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed key ->
            case key of
                Unsupported ->
                    ( model, Cmd.none )

                Escape ->
                    ( { model
                        | cells = Array.map (\cell -> { cell | isActive = False }) model.cells
                      }
                    , Cmd.none
                    )

                ArrowDown ->
                    case findActiveCell model.cells of
                        Just ( index, cell ) ->
                            let
                                updated =
                                    { cell | cursorPosition = updateLine 1 cell.cursorPosition }
                            in
                            ( { model | cells = Array.set index updated model.cells }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ArrowLeft ->
                    case findActiveCell model.cells of
                        Just ( index, cell ) ->
                            let
                                updated =
                                    { cell | cursorPosition = updateColumn -1 cell.cursorPosition }
                            in
                            ( { model | cells = Array.set index updated model.cells }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ArrowRight ->
                    case findActiveCell model.cells of
                        Just ( index, cell ) ->
                            let
                                updated =
                                    { cell | cursorPosition = updateColumn 1 cell.cursorPosition }
                            in
                            ( { model | cells = Array.set index updated model.cells }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                ArrowUp ->
                    case findActiveCell model.cells of
                        Just ( index, cell ) ->
                            let
                                updated =
                                    { cell | cursorPosition = updateLine -1 cell.cursorPosition }
                            in
                            ( { model | cells = Array.set index updated model.cells }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

        CellFocusOk _ ->
            ( model, Cmd.none )

        CellFocusErr _ ->
            ( model, Cmd.none )

        CellActive index ->
            let
                toFocusMsg : Result Dom.Error () -> Msg
                toFocusMsg result =
                    case result of
                        Ok _ ->
                            CellFocusOk index

                        Err _ ->
                            CellFocusErr index

                focusTask : Task.Task Dom.Error ()
                focusTask =
                    Dom.focus ("cell-input-" ++ String.fromInt index)
            in
            case Array.get index model.cells of
                Just cell ->
                    let
                        updated =
                            { cell | isActive = True }
                    in
                    ( { cells = Array.set index updated model.cells
                      }
                    , Task.attempt toFocusMsg focusTask
                    )

                Nothing ->
                    ( model, Cmd.none )

        CellUpdated index raw ->
            case Array.get index model.cells of
                Just cell ->
                    let
                        updatedLines =
                            String.split "\n" raw

                        updated =
                            { cell
                                | cursorPosition = getCursorPosition cell
                                , raw = raw
                                , lines = updatedLines
                            }
                    in
                    ( { cells = Array.set index updated model.cells
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


getCursorPosition : Cell -> SrcLoc
getCursorPosition cell =
    let
        default =
            { column = 1, line = 1 }

        worker : Char -> SrcLoc -> SrcLoc
        worker char curr =
            case char of
                '\n' ->
                    { column = 1, line = 1 }

                _ ->
                    { curr | column = curr.column + 1 }
    in
    String.foldl worker default cell.raw


view : Model -> Html.Html Msg
view model =
    Html.div
        [ A.classList [ ( "document", True ) ] ]
        (List.map cellView <| Array.toIndexedList model.cells)


cellView : ( Int, Cell ) -> Html.Html Msg
cellView ( index, cell ) =
    case cell.content of
        Code ->
            Html.textarea [] []

        Text ->
            textCellView ( index, cell )


textCellView : ( Int, Cell ) -> Html.Html Msg
textCellView ( index, cell ) =
    Html.div
        [ A.classList
            [ ( "editor", True )
            , ( "editor-active", cell.isActive )
            ]
        , E.onClick (CellActive index)
        ]
        [ Html.textarea
            [ A.classList [ ( "editor-input", True ) ]
            , A.id <| "cell-input-" ++ String.fromInt index
            , E.onInput (CellUpdated index)
            ]
            [ Html.text cell.raw ]
        , Html.div
            [ A.classList [ ( "editor-display", True ) ] ]
            [ Markdown.toHtml [] cell.raw ]
        ]
