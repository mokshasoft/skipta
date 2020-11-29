module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as E
import PwHash as Pw
import String as S



-- CONFIG


{-| App salt that should be unique to this app.
-}
appSalt : String
appSalt =
    "y+NhmrSICcOLoyqch+X4IneTh8rqZRJzYPeTn3s0sDP1VB7BNsxQGQM7QapAbqF8roFd7qij+U2W5g10AF527A=="


backendServerUrl : String
backendServerUrl =
    "http://localhost:3000/"


signInEndpoint : String
signInEndpoint =
    backendServerUrl ++ "signin"


joinNowEndpoint : String
joinNowEndpoint =
    backendServerUrl ++ "joinnow"



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
    { tabState : Tab.State
    , siEmail : String
    , siPassword : String
    , jnEmail1 : String
    , jnEmail2 : String
    , jnPassword1 : String
    , jnPassword2 : String
    , debugMsg : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tabState = Tab.initialState
      , siEmail = ""
      , siPassword = ""
      , jnEmail1 = ""
      , jnEmail2 = ""
      , jnPassword1 = ""
      , jnPassword2 = ""
      , debugMsg = ""
      }
    , Cmd.none
    )



-- HTTP


encodeSignIn : String -> String -> E.Value
encodeSignIn salt hashedPassword =
    E.object
        [ ( "salt", E.string salt )
        , ( "password", E.string hashedPassword )
        ]


sendSignIn : String -> String -> Cmd Msg
sendSignIn email password =
    let
        ( salt, hashedPassword ) =
            Pw.pwhash appSalt email password
    in
    Http.post
        { url = signInEndpoint
        , body = Http.jsonBody (encodeSignIn salt hashedPassword)
        , expect = Http.expectWhatever ReceiveSignIn
        }


encodeJoinNow : String -> String -> E.Value
encodeJoinNow salt hashedPassword =
    E.object
        [ ( "salt", E.string salt )
        , ( "password", E.string hashedPassword )
        ]


sendJoinNow : String -> String -> Cmd Msg
sendJoinNow email password =
    let
        ( salt, hashedPassword ) =
            Pw.pwhash appSalt email password
    in
    Http.post
        { url = joinNowEndpoint
        , body = Http.jsonBody (encodeJoinNow salt hashedPassword)
        , expect = Http.expectWhatever ReceiveJoinNow
        }



-- UPDATE


type Msg
    = TabMsg Tab.State
    | SiEmail String
    | SiPassword String
    | JnEmail1 String
    | JnEmail2 String
    | JnPassword1 String
    | JnPassword2 String
    | SendSignIn
    | ReceiveSignIn (Result Http.Error ())
    | SendJoinNow
    | ReceiveJoinNow (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        SiEmail email ->
            ( { model
                | siEmail = email
              }
            , Cmd.none
            )

        SiPassword pass ->
            ( { model
                | siPassword = pass
              }
            , Cmd.none
            )

        JnEmail1 email ->
            ( { model
                | jnEmail1 = email
              }
            , Cmd.none
            )

        JnEmail2 email ->
            ( { model
                | jnEmail2 = email
              }
            , Cmd.none
            )

        JnPassword1 pass ->
            ( { model
                | jnPassword1 = pass
              }
            , Cmd.none
            )

        JnPassword2 pass ->
            ( { model
                | jnPassword2 = pass
              }
            , Cmd.none
            )

        SendSignIn ->
            ( model
            , sendSignIn model.siEmail model.siPassword
            )

        ReceiveSignIn result ->
            case result of
                Ok _ ->
                    ( { model | debugMsg = "Sign in sent" }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | debugMsg = "Failed to sign in" }
                    , Cmd.none
                    )

        SendJoinNow ->
            ( model
            , sendJoinNow model.jnEmail1 model.jnPassword1
            )

        ReceiveJoinNow result ->
            case result of
                Ok _ ->
                    ( { model | debugMsg = "Join now sent" }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | debugMsg = "Failed to join now" }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Tab.subscriptions model.tabState TabMsg



-- VIEW


disableSignIn : Model -> Bool
disableSignIn model =
    (S.length model.siEmail == 0)
        || (S.length model.siPassword == 0)


disableJoinNow : Model -> Bool
disableJoinNow model =
    (S.length model.jnEmail1 == 0)
        || (S.length model.jnPassword1 == 0)
        || (model.jnEmail1 /= model.jnEmail2)
        || (model.jnPassword1 /= model.jnPassword2)


viewSignIn : Model -> Html Msg
viewSignIn model =
    Grid.container [ class "text-center m-3" ]
        [ Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-1" ]
                    [ Input.email [ Input.onInput SiEmail, Input.placeholder "E-mail" ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-1" ]
                    [ Input.password [ Input.onInput SiPassword, Input.placeholder "Password" ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-3" ]
                    [ Button.button
                        [ Button.onClick SendSignIn
                        , Button.primary
                        , Button.disabled (disableSignIn model)
                        ]
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
                            Pw.pwhash appSalt model.siEmail model.siPassword
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


viewJoinNow : Model -> Html Msg
viewJoinNow model =
    Grid.container [ class "text-center m-3" ]
        [ Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-1" ]
                    [ Input.email [ Input.onInput JnEmail1, Input.placeholder "E-mail" ]
                    , Input.email [ Input.onInput JnEmail2, Input.placeholder "Reenter e-mail" ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-1" ]
                    [ Input.password [ Input.onInput JnPassword1, Input.placeholder "Password" ]
                    , Input.password [ Input.onInput JnPassword2, Input.placeholder "Reenter password" ]
                    ]
                ]
            ]
        , Grid.row []
            [ Grid.col []
                [ div
                    [ class "m-3" ]
                    [ Button.button
                        [ Button.onClick SendJoinNow
                        , Button.primary
                        , Button.disabled (disableJoinNow model)
                        ]
                        [ text "Join now" ]
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Grid.row []
            [ Grid.col []
                [ div []
                    [ Tab.config TabMsg
                        |> Tab.withAnimation
                        |> Tab.center
                        |> Tab.items
                            [ Tab.item
                                { id = "tabLogin"
                                , link = Tab.link [] [ text "Sign in" ]
                                , pane = Tab.pane [] [ viewSignIn model ]
                                }
                            , Tab.item
                                { id = "tabSignUp"
                                , link = Tab.link [] [ text "Join now" ]
                                , pane = Tab.pane [] [ viewJoinNow model ]
                                }
                            ]
                        |> Tab.view model.tabState
                    ]
                ]
            ]
        ]
