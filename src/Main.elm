module Main exposing (Board, Cell(..), Model, Msg(..), Row, init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)



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
    ( ( Empty, Empty, Empty )
    , ( Empty, Empty, Empty )
    , ( Empty, Empty, Empty )
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
    div []
        [ img [ src "/logo.svg" ] []
        , renderBoard model.board
        ]


renderBoard : Board -> Html Msg
renderBoard ( ( topLeft, topCenter, topRight ), ( middleLeft, middleCenter, middleRight ), ( bottomLeft, bottomCenter, bottomRight ) ) =
    div []
        [ div []
            [ text "Top"
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
