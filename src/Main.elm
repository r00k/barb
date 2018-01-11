port module Main exposing (..)

-- Todos
-- don't expose everything
-- consistency around maybe/sometimes
-- no magic numbers

import Collage
import Element
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Polygon exposing (..)
import Random exposing (Generator)
import Task
import Process
import Exts.Float


-- MODEL


type alias Model =
    { bestCandidate : Image
    , bestCandidateFitness : Float
    , candidate : Image
    , candidateFitness : Float
    , iterations : Int
    , pixelValuesForGoalImage : Pixels
    , imageHeight : Int
    , imageWidth : Int
    , hasStarted : Bool
    }


type alias Pixels =
    List Int


imageDimension : Int
imageDimension =
    100


init : ( Model, Cmd Msg )
init =
    ( { bestCandidate = []
      , bestCandidateFitness = 0.0
      , candidate = []
      , candidateFitness = 0.0
      , iterations = 0
      , pixelValuesForGoalImage = []
      , imageHeight = imageDimension
      , imageWidth = imageDimension
      , hasStarted = False
      }
    , Cmd.none
    )


checkFitness : Pixels -> Pixels -> Float
checkFitness goalImage candidateImage =
    let
        pixelCount =
            List.length goalImage

        differences =
            List.map2 (-) goalImage candidateImage

        squares =
            List.map (\x -> x ^ 2) differences

        sumOfSquares =
            List.sum squares |> toFloat

        maximumDifference =
            pixelCount * 256 * 256 |> toFloat
    in
        1 - (sumOfSquares / maximumDifference)



-- UPDATE


type Msg
    = CalculateFitness Pixels
    | Start
    | RequestCandidateImage
    | StoreGoalImage Pixels
    | UpdateCandidate Image
    | Sleep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | hasStarted = True }
            , requestGoalImage ()
            )

        StoreGoalImage pixelValues ->
            ( { model
                | pixelValuesForGoalImage = pixelValues
              }
            , Random.generate UpdateCandidate randomImage
            )

        UpdateCandidate image ->
            update Sleep { model | candidate = image }

        Sleep ->
            ( model, Task.perform (always RequestCandidateImage) (Process.sleep 0) )

        RequestCandidateImage ->
            ( model, requestCandidateImage () )

        CalculateFitness candidateImage ->
            let
                newCandidateFitness =
                    checkFitness model.pixelValuesForGoalImage candidateImage
            in
                if newCandidateFitness > model.bestCandidateFitness then
                    ( { model
                        | bestCandidate = model.candidate
                        , bestCandidateFitness = newCandidateFitness
                        , iterations = model.iterations + 1
                        , candidateFitness = newCandidateFitness
                      }
                    , Random.generate UpdateCandidate (mutateImage model.candidate)
                    )
                else
                    ( { model
                        | candidateFitness = newCandidateFitness
                        , candidate = model.bestCandidate
                        , iterations = model.iterations + 1
                      }
                    , Random.generate UpdateCandidate (mutateImage model.bestCandidate)
                    )



-- VIEW


applyGoalImageSize : Model -> Attribute msg
applyGoalImageSize model =
    style
        [ ( "width", (toString model.imageWidth) ++ "px" )
        , ( "height", (toString model.imageHeight) ++ "px" )
        ]


displayablePercentage : Float -> String
displayablePercentage number =
    let
        rounded =
            Exts.Float.roundTo 2 (number * 100)
    in
        (toString rounded) ++ "%"


renderStartAndInfo : Model -> Html Msg
renderStartAndInfo model =
    if model.hasStarted then
        div
            [ class "images-image_container-info_tray" ]
            [ div
                [ class "images-image_container-info_tray-number" ]
                [ text <| toString model.iterations ]
            , div
                [ class "images-image_container-info_tray-number" ]
                [ text <| displayablePercentage model.bestCandidateFitness ]
            ]
    else
        div
            [ Html.Events.onClick Start
            , class "images-image_container-info_tray images-image_container-info_tray--button"
            ]
            [ text "Start" ]


view : Model -> Html Msg
view model =
    div [ class "images" ]
        [ div
            [ class "images-image_container" ]
            [ img [ src "img/quarters.jpg", class "images-original_image_container-image" ] [] ]
        , div
            [ class "images-image_container" ]
            [ div
                [ applyGoalImageSize model
                , class "images-image_container-generated_image_canvas class images-image_container-force_size_fill"
                ]
                [ drawCandidate model ]
            , renderStartAndInfo model
            ]
        ]


drawCandidate : Model -> Html Msg
drawCandidate model =
    Collage.collage
        (round ((toFloat model.imageWidth) / 2))
        (round ((toFloat model.imageHeight) / 2))
        (List.map drawPolygon model.candidate)
        |> Element.toHtml


drawPolygon : Polygon -> Collage.Form
drawPolygon polygon =
    Collage.polygon polygon.vertices
        |> Collage.filled polygon.color



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ goalImage StoreGoalImage
        , candidateImage CalculateFitness
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


port requestGoalImage : () -> Cmd msg


port goalImage : (Pixels -> msg) -> Sub msg


port requestCandidateImage : () -> Cmd msg


port candidateImage : (Pixels -> msg) -> Sub msg
