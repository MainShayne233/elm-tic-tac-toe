module Main exposing (Board, Cell(..), Model, Msg(..), Row, init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, p, text)
import Html.Attributes exposing (src, style)



---- MODEL ----


type Cell
    = X
    | O
    | Empty


type alias Row =
    ( Cell, Cell, Cell )


type alias Board =
    ( Row, Row, Row )


type alias Model =
    { board : Board }


initBoard : Board
initBoard =
    ( ( X, X, O )
    , ( O, X, X )
    , ( X, O, X )
    )


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


renderCell : Cell -> Html Msg
renderCell cell =
    let
        textContent =
            case cell of
                X ->
                    "X"

                O ->
                    "O"

                Empty ->
                    " "
    in
    p cellStyle [ text textContent ]


renderRow : Row -> Html Msg
renderRow ( cell1, cell2, cell3 ) =
    div rowStyle
        (List.map renderCell [ cell1, cell2, cell3 ])


renderBoard : Board -> Html Msg
renderBoard ( topRow, middleRow, bottomRow ) =
    div boardStyle
        (List.map
            renderRow
            [ topRow, middleRow, bottomRow ]
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
