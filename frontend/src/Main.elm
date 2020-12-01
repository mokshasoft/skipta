module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Navbar
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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { authVisibility : Modal.Visibility
    , navbarState : Navbar.State
    , tabState : Tab.State
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
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
    in
    ( { authVisibility = Modal.hidden
      , navbarState = navbarState
      , tabState = Tab.initialState
      , siEmail = ""
      , siPassword = ""
      , jnEmail1 = ""
      , jnEmail2 = ""
      , jnPassword1 = ""
      , jnPassword2 = ""
      , debugMsg = ""
      }
    , navbarCmd
    )



-- HTTP


encodeAuthentication : String -> String -> E.Value
encodeAuthentication salt hashedPassword =
    E.object
        [ ( "email", E.string salt )
        , ( "hashedPassword", E.string hashedPassword )
        ]


sendSignIn : String -> String -> Cmd Msg
sendSignIn email password =
    let
        hashedPassword : String
        hashedPassword =
            Pw.pwhash appSalt email password
    in
    Http.post
        { url = signInEndpoint
        , body = Http.jsonBody (encodeAuthentication email hashedPassword)
        , expect = Http.expectWhatever ReceiveSignIn
        }


sendJoinNow : String -> String -> Cmd Msg
sendJoinNow email password =
    let
        hashedPassword : String
        hashedPassword =
            Pw.pwhash appSalt email password
    in
    Http.post
        { url = joinNowEndpoint
        , body = Http.jsonBody (encodeAuthentication email hashedPassword)
        , expect = Http.expectWhatever ReceiveJoinNow
        }



-- UPDATE


type Msg
    = AuthModal Modal.Visibility
    | NavbarMsg Navbar.State
    | TabMsg Tab.State
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
        AuthModal state ->
            ( { model
                | authVisibility = state
              }
            , Cmd.none
            )

        NavbarMsg state ->
            ( { model | navbarState = state }
            , Cmd.none
            )

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
    Sub.batch
        [ Tab.subscriptions model.tabState TabMsg
        , Navbar.subscriptions model.navbarState NavbarMsg
        ]



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
                        , Button.outlinePrimary
                        , Button.disabled (disableSignIn model)
                        ]
                        [ text "Login" ]
                    ]
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
                        , Button.outlinePrimary
                        , Button.disabled (disableJoinNow model)
                        ]
                        [ text "Join now" ]
                    ]
                ]
            ]
        ]


viewAuthModal : Model -> Html Msg
viewAuthModal model =
    Grid.container []
        [ Button.button
            [ Button.attrs [ onClick (AuthModal Modal.shown) ] ]
            [ text "Account" ]
        , Modal.config (AuthModal Modal.hidden)
            |> Modal.small
            |> Modal.h5 [] [ text "Sign In/Join Now" ]
            |> Modal.body []
                [ Grid.containerFluid []
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
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick (AuthModal Modal.hidden) ]
                    ]
                    [ text "Close" ]
                ]
            |> Modal.view model.authVisibility
        ]


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
        , Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#" ] [ img [ src "../assets/logo.svg", style "width" "50px", style "background-size" "contain" ] [], text "  Skipta" ]
            |> Navbar.customItems
                [ Navbar.textItem [] [ viewAuthModal model ] ]
            |> Navbar.view model.navbarState
        ]
