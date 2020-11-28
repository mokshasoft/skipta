module Main exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as E
import PwHash as Pw



-- CONFIG


{-| App salt that should be unique to this app.
-}
appSalt : String
appSalt =
    "y+NhmrSICcOLoyqch+X4IneTh8rqZRJzYPeTn3s0sDP1VB7BNsxQGQM7QapAbqF8roFd7qij+U2W5g10AF527A=="


backendServerUrl : String
backendServerUrl =
    "http://localhost:3000/"


loginEndpoint : String
loginEndpoint =
    backendServerUrl ++ "login"



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { email : String
    , password : String
    , debugMsg : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { email = ""
      , password = ""
      , debugMsg = ""
      }
    , Cmd.none
    )



-- HTTP


encodeLogin : String -> String -> E.Value
encodeLogin salt hashedPassword =
    E.object
        [ ( "salt", E.string salt )
        , ( "password", E.string hashedPassword )
        ]


sendLogin : String -> String -> Cmd Msg
sendLogin email password =
    let
        ( salt, hashedPassword ) =
            Pw.pwhash appSalt email password
    in
    Http.post
        { url = loginEndpoint
        , body = Http.jsonBody (encodeLogin salt hashedPassword)
        , expect = Http.expectWhatever ReceiveLogin
        }



-- UPDATE


type Msg
    = ChangeEmail String
    | ChangePassword String
    | SendLogin
    | ReceiveLogin (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            ( { model
                | email = email
              }
            , Cmd.none
            )

        ChangePassword pass ->
            ( { model
                | password = pass
              }
            , Cmd.none
            )

        SendLogin ->
            ( model
            , sendLogin model.email model.password
            )

        ReceiveLogin result ->
            case result of
                Ok _ ->
                    ( { model | debugMsg = "Login sent" }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | debugMsg = "Failed to login" }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ div
                    []
                    [ input [ onInput ChangeEmail, placeholder "email" ]
                        []
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    []
                    [ input [ onInput ChangePassword, placeholder "password" ]
                        []
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    []
                    [ button [ onClick SendLogin ]
                        [ text "Login" ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    []
                    (let
                        ( salt, hashedPassword ) =
                            Pw.pwhash appSalt model.email model.password
                     in
                     [ label []
                        [ text <| "salt: " ++ salt ]
                     , label []
                        [ text <| "hash: " ++ hashedPassword ]
                     ]
                    )
                , label []
                    [ text <| "debug message: " ++ model.debugMsg ]
                ]
            ]
        ]
