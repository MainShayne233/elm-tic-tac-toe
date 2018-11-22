module Main exposing (Board, Cell(..), Model, Msg(..), Row, init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)



---- MODEL ----


type Cell
    = X
    | O
    | Empty


type alias Row =
    Array Cell


type alias Board =
    Array Row


type alias Model =
    { board : Board }


initCell : Cell
initCell =
    Empty


initRow : Row
initRow =
    Array.initialize 3 (always initCell)


initBoard : Board
initBoard =
    Array.initialize 3 (always initRow)


init : ( Model, Cmd Msg )
init =
    ( { board = initBoard }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div containerStyle
        [ h1 []
            [ text "Tic Tac Toe" ]
        , renderBoard
            model.board
        ]


getCell : Int -> Int -> Board -> Cell
getCell rowIndex columnIndex board =
    Array.get rowIndex board
        |> Maybe.withDefault initRow
        |> Array.get columnIndex
        |> Maybe.withDefault initCell


renderCell : Int -> Int -> Board -> Html Msg
renderCell rowIndex columnIndex board =
    let
        textContent =
            case getCell rowIndex columnIndex board of
                X ->
                    "X"

                O ->
                    "O"

                Empty ->
                    " "
    in
    button cellStyle [ text textContent ]


renderRow : Int -> Board -> Html Msg
renderRow rowIndex board =
    div rowStyle
        (List.range
            0
            2
            |> List.map (\columnIndex -> renderCell rowIndex columnIndex board)
        )


renderBoard : Board -> Html Msg
renderBoard board =
    div boardStyle
        (List.range
            0
            2
            |> List.map (\rowIndex -> renderRow rowIndex board)
        )



---- STYLE -----


containerStyle : List (Html.Attribute Msg)
containerStyle =
    [ style "display" "flex", style "justify-content" "center", style "flex-direction" "column", style "align-items" "center" ]


cellStyle : List (Html.Attribute Msg)
cellStyle =
    [ style "height" "100px", style "width" "100px", style "border" "solid", style "padding" "0", style "margin" "0", style "font-size" "80px" ]


rowStyle : List (Html.Attribute Msg)
rowStyle =
    [ style "display" "flex" ]


boardStyle : List (Html.Attribute Msg)
boardStyle =
    [ style "display" "flex", style "flex-direction" "column" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
