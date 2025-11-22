port module Main exposing (main)

{-| 256t.org CID Examples - Elm Implementation

This Elm application displays CID (Content Identifier) examples and allows users
to verify them against the 256t.org specification.

-}

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)



-- PORTS


port computeCid : String -> Cmd msg


port cidComputed : (CidResult -> msg) -> Sub msg


type alias CidResult =
    { cid : String
    , length : Int
    }



-- MODEL


type alias Model =
    { cidFiles : List CidFile
    , selectedCid : Maybe String
    , selectedContent : Maybe String
    , calculatedCid : Maybe String
    , referenceCid : Maybe String
    , loadingState : LoadingState
    , error : Maybe String
    }


type alias CidFile =
    { cid : String
    , cidPreview : String
    , textPreview : String
    , length : Int
    }


type LoadingState
    = Loading
    | Loaded
    | Failed


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cidFiles = []
      , selectedCid = Nothing
      , selectedContent = Nothing
      , calculatedCid = Nothing
      , referenceCid = Nothing
      , loadingState = Loading
      , error = Nothing
      }
    , loadCidList
    )



-- UPDATE


type Msg
    = CidListLoaded (Result Http.Error (List CidFile))
    | SelectCid String
    | ContentLoaded String (Result Http.Error String)
    | CidResultReceived CidResult


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CidListLoaded result ->
            case result of
                Ok cidFiles ->
                    ( { model
                        | cidFiles = cidFiles
                        , loadingState = Loaded
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model
                        | loadingState = Failed
                        , error = Just "Failed to load CID list"
                      }
                    , Cmd.none
                    )

        SelectCid cid ->
            ( { model
                | selectedCid = Just cid
                , referenceCid = Just cid
                , selectedContent = Nothing
                , calculatedCid = Nothing
              }
            , loadContent cid
            )

        ContentLoaded cid result ->
            case result of
                Ok content ->
                    ( { model | selectedContent = Just content }
                    , computeCid content
                    )

                Err _ ->
                    ( { model | error = Just "Failed to load content" }
                    , Cmd.none
                    )

        CidResultReceived result ->
            ( { model | calculatedCid = Just result.cid }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    cidComputed CidResultReceived



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h1 [] [ text "CID Examples (Elm)" ]
            , p [] [ text "Select a text file to calculate and verify its Content Identifier (CID) using the Elm implementation." ]
            , nav []
                [ a [ href "index.html" ] [ text "← Readme" ]
                , a [ href "examples.html" ] [ text "Examples (JS)" ]
                , a [ href "hash.html" ] [ text "Hash Calculator" ]
                , a [ href "check.html" ] [ text "File CID Check" ]
                , a [ href "download.html" ] [ text "CID Download" ]
                ]
            ]
        , div []
            [ h2 [ style "font-weight" "600", style "margin-bottom" "0.5rem" ]
                [ text "Select a CID file:" ]
            , viewCidGrid model
            ]
        , viewFullContent model
        , viewResult model
        ]


viewCidGrid : Model -> Html Msg
viewCidGrid model =
    case model.loadingState of
        Loading ->
            div [ class "button-grid" ] [ text "Loading..." ]

        Failed ->
            div [ class "button-grid" ]
                [ text (Maybe.withDefault "Error loading files" model.error) ]

        Loaded ->
            div [ class "button-grid" ]
                (List.map viewCidButton model.cidFiles)


viewCidButton : CidFile -> Html Msg
viewCidButton cidFile =
    button
        [ class "cid-button"
        , onClick (SelectCid cidFile.cid)
        ]
        [ div [ class "cid-preview" ]
            [ text (cidFile.cidPreview ++ "...") ]
        , div [ class "cid-content" ]
            [ text cidFile.textPreview ]
        ]


viewFullContent : Model -> Html Msg
viewFullContent model =
    case model.selectedContent of
        Nothing ->
            div [ class "full-content hidden" ] []

        Just content ->
            div [ class "full-content" ]
                [ h3 [] [ text "Selected Content" ]
                , div [ class "full-content-text" ] [ text content ]
                ]


viewResult : Model -> Html Msg
viewResult model =
    case ( model.referenceCid, model.calculatedCid, model.selectedContent ) of
        ( Just refCid, Just calcCid, Just content ) ->
            let
                matches =
                    refCid == calcCid

                statusText =
                    if matches then
                        "CID matches!"

                    else
                        "CID does not match"

                statusClass =
                    if matches then
                        "success"

                    else
                        "failure"
            in
            div [ class "result" ]
                [ div [ class "status" ]
                    [ span [ class ("status-icon " ++ statusClass) ] []
                    , text statusText
                    ]
                , div [ class "info-grid" ]
                    [ div [ class "info-item" ]
                        [ div [ class "info-label" ] [ text "Reference CID (from filename)" ]
                        , div [ class "info-value" ] [ text refCid ]
                        ]
                    , div [ class "info-item" ]
                        [ div [ class "info-label" ] [ text "Calculated CID" ]
                        , div [ class "info-value" ] [ text calcCid ]
                        ]
                    , div [ class "info-item" ]
                        [ div [ class "info-label" ] [ text "Content Preview (first 100 characters)" ]
                        , div [ class "info-value" ] [ text (String.left 100 content) ]
                        ]
                    , div [ class "info-item" ]
                        [ div [ class "info-label" ] [ text "Content Length" ]
                        , div [ class "info-value" ] [ text (String.fromInt (String.length content) ++ " bytes") ]
                        ]
                    ]
                , div [ class "code-section" ]
                    [ h3 [] [ text "Elm Implementation" ]
                    , pre []
                        [ code []
                            [ text """-- Compute CID from content
computeCid : String -> String
computeCid content =
    let
        length = String.length content
        prefix = encodeLength length
    in
    if length <= 64 then
        prefix ++ toBase64Url content
    else
        prefix ++ sha512ToBase64Url content"""
                            ]
                        ]
                    , a [ href "implementations/elm/src/Main.elm", class "code-link", target "_blank" ]
                        [ text "View full source code →" ]
                    ]
                ]

        _ ->
            div [ class "result hidden" ] []



-- HTTP


loadCidList : Cmd Msg
loadCidList =
    Http.get
        { url = "cids.json"
        , expect = Http.expectJson CidListLoaded cidListDecoder
        }


loadContent : String -> Cmd Msg
loadContent cid =
    Http.get
        { url = "cids/" ++ cid
        , expect = Http.expectString (ContentLoaded cid)
        }


cidListDecoder : Decoder (List CidFile)
cidListDecoder =
    list
        (Decode.map4 CidFile
            (field "cid" string)
            (field "cidPreview" string)
            (field "textPreview" string)
            (field "length" int)
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
