module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List exposing (indexedMap, map, range, repeat)
import Process
import Random
import Random.List exposing (shuffle)
import String exposing (fromInt)
import Task


type alias Model =
    { cellValues : List Int
    , hidden : Bool
    , previousClickedValue : Int
    }


type Msg
    = ClickOn Int
    | NewCellValues (List Int)


initialModel : Model
initialModel =
    { cellValues = repeat 70 -1
    , hidden = False
    , previousClickedValue = -1
    }


generateCellValues : Cmd Msg
generateCellValues =
    Random.generate NewCellValues (shuffle (range 0 9 ++ repeat 60 -1))


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, generateCellValues )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


updateCellValue : Int -> Int -> Int
updateCellValue storedVal val =
    if val == storedVal then
        -1
    else
        val


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickOn val ->
            if val /= -1 then
                if val == model.previousClickedValue + 1 then
                    ( { model
                        | hidden = True
                        , previousClickedValue = val
                        , cellValues = map (updateCellValue val) model.cellValues
                      }
                    , Cmd.none
                    )

                else
                    init ()

            else
                ( model, Cmd.none )

        NewCellValues newCellValues ->
            ( { model | cellValues = newCellValues }, Cmd.none )


cell : Int -> Int -> Html Msg
cell i val =
    let
        content =
            if val /= -1 then
                fromInt val

            else
                ""

        classes =
            classList
                [ ( "cell", True )
                , ( "cell-with-value", val /= -1 )
                ]
    in
    div
        [ classes, onClick (ClickOn val) ]
        [ text content ]


view : Model -> Html Msg
view model =
    div []
        [ div
            [ classList
                [ ( "tbl", True )
                , ( "hidden", model.hidden )
                ]
            ]
            (indexedMap cell model.cellValues)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

