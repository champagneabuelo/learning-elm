module App exposing (..)

import Html exposing (Html, button, div, text, program)
import Html.Events exposing (onClick)
import Random

-- Model

type alias Model =
    Int

init : ( Model, Cmd Msg)
init =
    ( 1, Cmd.none )


-- Messages

type Msg
    = Roll
    | OnResult Int

-- View

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ text "Roll" ]
        , text (toString model)
        ]

-- Update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll->
            ( model, Random.generate OnResult (Random.int 1 6) )
        OnResult res ->
            ( res, Cmd.none )

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- Main

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
