module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List exposing (indexedMap, map, range, repeat)
import Process
import Random
import Random.List exposing (shuffle)
import SingleSlider
import String exposing (fromInt)
import Task


type alias Model =
    { cellValues : List Int
    , hidden : Bool
    , previousClickedValue : Int
    , delaySlider : SingleSlider.Model
    }


type Msg
    = ClickOn Int
    | Hide
    | NewCellValues (List Int)
    | DelaySliderMsg SingleSlider.Msg


initialModel : Model
initialModel =
    { cellValues = repeat 70 -1
    , hidden = False
    , previousClickedValue = -1
    , delaySlider =
        let
            def =
                SingleSlider.defaultModel
        in
        { def
            | min = 10
            , max = 3000
            , step = 10
            , value = 1000
        }
    }


initialCommands : Float -> Cmd Msg
initialCommands delayTime =
    Cmd.batch [ generateCellValues, delay delayTime Hide ]


generateCellValues : Cmd Msg
generateCellValues =
    Random.generate NewCellValues (shuffle (range 0 9 ++ repeat 60 -1))


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


init : Model -> () -> ( Model, Cmd Msg )
init model _ =
    ( model, initialCommands <| model.delaySlider.value )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map DelaySliderMsg <| SingleSlider.subscriptions model.delaySlider


updateCellValue : Int -> Int -> Int
updateCellValue storedVal val =
    if val == storedVal then
        -1

    else
        val


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Hide ->
            ( { model | hidden = True }, Cmd.none )

        ClickOn val ->
            if model.hidden && val /= -1 then
                if val == model.previousClickedValue + 1 then
                    ( { model
                        | previousClickedValue = val
                        , cellValues = map (updateCellValue val) model.cellValues
                      }
                    , Cmd.none
                    )

                else
                    init { initialModel | delaySlider = model.delaySlider } ()

            else
                ( model, Cmd.none )

        NewCellValues newCellValues ->
            ( { model | cellValues = newCellValues }, Cmd.none )

        DelaySliderMsg sliderMsg ->
            let
                ( newSlider, cmd, updateResults ) =
                    SingleSlider.update sliderMsg model.delaySlider

                newModel =
                    { model
                        | delaySlider = newSlider
                        , hidden = False
                    }

                newCmd =
                    if updateResults then
                        Cmd.batch [ Cmd.map DelaySliderMsg cmd ]

                    else
                        Cmd.none
            in
            ( newModel, Cmd.batch [ newCmd, initialCommands model.delaySlider.value ] )


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
        , SingleSlider.view model.delaySlider
            |> Html.map DelaySliderMsg
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

