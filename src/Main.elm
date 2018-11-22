module Main exposing (Board, Cell(..), Model, Msg(..), Row, init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, p, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)



---- MODEL ----


type Player
    = PlayerX
    | PlayerO


type Cell
    = X
    | O
    | Empty


type alias Row =
    Array Cell


type alias Board =
    Array Row


type alias Model =
    { board : Board, currentPlayer : Player }


initCurrentPlayer : Player
initCurrentPlayer =
    PlayerX


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
    ( { board = initBoard, currentPlayer = initCurrentPlayer }, Cmd.none )



---- UPDATE ----


type Msg
    = CellClick Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CellClick rowIndex columnIndex ->
            let
                newCell =
                    cellForPlayer model.currentPlayer

                otherPlayer =
                    opposingPlayer model.currentPlayer
            in
            ( { model | board = setCell rowIndex columnIndex newCell model.board, currentPlayer = otherPlayer }, Cmd.none )


cellForPlayer : Player -> Cell
cellForPlayer player =
    case player of
        PlayerX ->
            X

        PlayerO ->
            O


opposingPlayer : Player -> Player
opposingPlayer player =
    case player of
        PlayerX ->
            PlayerO

        PlayerO ->
            PlayerX


setCell : Int -> Int -> Cell -> Board -> Board
setCell rowIndex columnIndex cell board =
    let
        row =
            getRow rowIndex board

        updatedRow =
            Array.set columnIndex cell row
    in
    Array.set rowIndex updatedRow board


getRow : Int -> Board -> Row
getRow rowIndex board =
    Array.get rowIndex board
        |> Maybe.withDefault initRow


getCell : Int -> Int -> Board -> Cell
getCell rowIndex columnIndex board =
    getRow rowIndex board
        |> Array.get columnIndex
        |> Maybe.withDefault initCell



---- VIEW ----


view : Model -> Html Msg
view model =
    div containerStyle
        [ h1 []
            [ text "Tic Tac Toe" ]
        , renderBoard
            model.board
        , renderTurn model
        ]


renderTurn : Model -> Html Msg
renderTurn { currentPlayer } =
    let
        textContent =
            case currentPlayer of
                PlayerX ->
                    "It's X's turn"

                PlayerO ->
                    "It's O's turn"
    in
    p [] [ text textContent ]


renderCell : Int -> Int -> Board -> Html Msg
renderCell rowIndex columnIndex board =
    let
        ( textContent, eventHandlers ) =
            case getCell rowIndex columnIndex board of
                X ->
                    ( "X", [] )

                O ->
                    ( "O", [] )

                Empty ->
                    ( " ", [ onClick (CellClick rowIndex columnIndex) ] )
    in
    button (cellStyle ++ eventHandlers) [ text textContent ]


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
