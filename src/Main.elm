port module Main exposing (..)

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
    { fittest : Image
    , fittestFitness : Float
    , candidate : Image
    , candidateFitness : Float
    , iterations : Int
    , imageDataForUploadedImage : List Int
    , imageHeight : Int
    , imageWidth : Int
    , hasStarted : Bool
    }


numberOfPolygons : Int
numberOfPolygons =
    125


init : ( Model, Cmd Msg )
init =
    ( { fittest = []
      , fittestFitness = 0.0
      , candidate = []
      , candidateFitness = 0.0
      , iterations = 0
      , imageDataForUploadedImage = []
      , imageHeight = 0
      , imageWidth = 0
      , hasStarted = False
      }
    , Cmd.none
    )


checkFitness : ( List Int, List Int ) -> Float
checkFitness ( uploadedImage, candidateImage ) =
    let
        pixelCount =
            List.length uploadedImage

        differences =
            List.map2 (-) uploadedImage candidateImage

        squares =
            List.map (\x -> x ^ 2) differences

        sumOfSquares =
            List.foldr (+) 0 squares

        maximumDifference =
            pixelCount * 256 * 256
    in
        1 - (toFloat sumOfSquares / toFloat (maximumDifference))


shiftList : List a -> a -> List a
shiftList existingList newListItem =
    List.append (List.drop 1 existingList) [ newListItem ]



-- UPDATE


type Msg
    = CalculateFitness (List Int)
    | Start
    | RequestCandidateImage
    | StoreUploadedImage (List Int)
    | UpdateCandidate Image
    | Sleep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | hasStarted = True }
            , requestUploadedImage ""
            )

        StoreUploadedImage image ->
            ( { model
                | imageDataForUploadedImage = image
                , imageHeight = 100
                , imageWidth = 100
              }
            , Random.generate UpdateCandidate (Random.list numberOfPolygons randomPolygon)
            )

        UpdateCandidate image ->
            update Sleep { model | candidate = image }

        Sleep ->
            ( model, Task.perform (always RequestCandidateImage) (Process.sleep 0) )

        RequestCandidateImage ->
            ( model, requestCandidateImage "" )

        CalculateFitness candidateImage ->
            let
                newCandidateFitness =
                    checkFitness ( model.imageDataForUploadedImage, candidateImage )
            in
                if newCandidateFitness > model.fittestFitness then
                    ( { model
                        | fittest = model.candidate
                        , fittestFitness = newCandidateFitness
                        , iterations = model.iterations + 1
                        , candidateFitness = newCandidateFitness
                      }
                    , Random.generate UpdateCandidate (mutatePolygons model.candidate)
                    )
                else
                    ( { model
                        | candidateFitness = newCandidateFitness
                        , candidate = model.fittest
                        , iterations = model.iterations + 1
                      }
                    , Random.generate UpdateCandidate (mutatePolygons model.fittest)
                    )



-- VIEW


applyUploadedImageSize : Model -> Attribute msg
applyUploadedImageSize model =
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


exaggeratePercentage : Float -> Float
exaggeratePercentage number =
    (((number * 100) - 90) * 10) / 100


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
                [ text <| displayablePercentage model.fittestFitness ]
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
                [ applyUploadedImageSize model
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
        [ uploadedImage StoreUploadedImage
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


port requestUploadedImage : String -> Cmd msg


port uploadedImage : (List Int -> msg) -> Sub msg


port requestCandidateImage : String -> Cmd msg


port candidateImage : (List Int -> msg) -> Sub msg
