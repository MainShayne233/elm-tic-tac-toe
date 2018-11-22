module Main exposing (Board, Cell(..), Model, Msg(..), Row, init, main, update, view)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



---- MODEL ----


type GameState
    = Playing
    | SomeoneWon Player
    | BoardIsFull


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
    { board : Board, currentPlayer : Player, gameState : GameState }


initGameState : GameState
initGameState =
    Playing


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
    ( { board = initBoard, currentPlayer = initCurrentPlayer, gameState = initGameState }, Cmd.none )



---- UPDATE ----


type Msg
    = CellClick Int Int
    | StartNewGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.gameState ) of
        ( CellClick rowIndex columnIndex, Playing ) ->
            let
                updatedModel =
                    model
                        |> applyCellClick rowIndex columnIndex
                        |> checkForGameStateChange
                        |> swapCurrentPlayer
            in
            ( updatedModel, Cmd.none )

        ( StartNewGame, _ ) ->
            ( { model | board = initBoard, currentPlayer = initCurrentPlayer, gameState = initGameState }, Cmd.none )

        other ->
            ( model, Cmd.none )


applyCellClick : Int -> Int -> Model -> Model
applyCellClick rowIndex columnIndex model =
    let
        row =
            getRow rowIndex model.board

        updatedRow =
            Array.set columnIndex (cellForPlayer model.currentPlayer) row
    in
    { model | board = Array.set rowIndex updatedRow model.board }


swapCurrentPlayer : Model -> Model
swapCurrentPlayer model =
    let
        otherPlayer =
            opposingPlayer model.currentPlayer
    in
    { model | currentPlayer = otherPlayer }


winningConfigurations : List (List ( Int, Int ))
winningConfigurations =
    [ [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
    , [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
    , [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ) ]
    , [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]
    , [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ) ]
    , [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
    , [ ( 2, 0 ), ( 1, 1 ), ( 0, 2 ) ]
    ]


checkConfigurationForWin : List ( Int, Int ) -> Model -> Bool
checkConfigurationForWin configuration { board, currentPlayer } =
    let
        winningCell =
            cellForPlayer currentPlayer

        currentPlayersCellsInConfiguration =
            configuration
                |> List.filter
                    (\( rowIndex, columnIndex ) ->
                        getCell rowIndex columnIndex board == winningCell
                    )
    in
    List.length currentPlayersCellsInConfiguration == 3


boardIsFull : Board -> Bool
boardIsFull board =
    List.range 0 2
        |> List.concatMap
            (\rowIndex ->
                List.range 0 2
                    |> List.map (\columnIndex -> getCell rowIndex columnIndex board)
            )
        |> List.all (\cell -> cell /= Empty)


checkForGameStateChange : Model -> Model
checkForGameStateChange model =
    if List.any (\configuration -> checkConfigurationForWin configuration model) winningConfigurations then
        { model | gameState = SomeoneWon model.currentPlayer }

    else if boardIsFull model.board then
        { model | gameState = BoardIsFull }

    else
        model


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


playerDisplayName : Player -> String
playerDisplayName player =
    case player of
        PlayerX ->
            "X"

        PlayerO ->
            "O"



---- VIEW ----


view : Model -> Html Msg
view model =
    div containerStyle
        [ h1 []
            [ text "Tic Tac Toe" ]
        , renderBoard
            model.board
        , renderDialog model
        ]


renderDialog : Model -> Html Msg
renderDialog { gameState, currentPlayer } =
    case gameState of
        SomeoneWon winningPlayer ->
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]
                [ p [] [ text (playerDisplayName winningPlayer ++ " won") ]
                , button [ onClick StartNewGame ] [ text "Play again?" ]
                ]

        BoardIsFull ->
            div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]
                [ p [] [ text "Stale mate!" ]
                , button [ onClick StartNewGame ] [ text "Play again?" ]
                ]

        Playing ->
            p [] [ text ("It's " ++ playerDisplayName currentPlayer ++ "'s turn") ]


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
