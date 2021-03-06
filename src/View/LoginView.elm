module View.LoginView exposing (loginView, viewNavBar)

import View.Styles exposing (..)
import Model.Model exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font

loginView: Model -> Element Msg
loginView model =
    case model.loginStatus of
        LoggedIn -> viewLoggedIn model
        Pending -> viewPending
        LoggedOut -> viewLoggedOut model
        Signup -> viewLoggedOut model


viewLoggedOut: Model -> Element Msg
viewLoggedOut model =
    column [ width fill, height fill ]
        [ viewNavBar model
        , viewLoggedOutRow model
        ]

viewLoggedIn: Model -> Element Msg
viewLoggedIn model = 
    viewNavBar model

viewPending: Element Msg
viewPending =
    row [ centerX, centerY ]
        [ text "We are processing your request"
        ]

viewNavBar: Model -> Element Msg
viewNavBar model = 
    let
        createOrLogoutButton = makeCreateOrLogOutButton model.loginStatus

        historyButton = 
            if model.loginStatus == LoggedIn
            then
                Input.button
                    (List.append buttonAttributes [ height <| px navBarHeight, alignRight ])
                    { onPress = Just ShowHistory
                    , label = el [centerX] <| text "History"
                    }
            else none
    in
        row [ Background.color primaryColor, width fill, spacing 10, paddingXY 10 5 ]
            [ Input.button
                (List.append [height <| px navBarHeight, alignLeft] buttonAttributes)
                { onPress = Just Home
                , label = el [ centerX ] <| text "Home"
                }
            , if model.window.width > 600
                then el [ centerX, Font.bold ] <| text "simple-timetracker.com"
                else none
            , historyButton
            , createOrLogoutButton
            ]

makeCreateOrLogOutButton: LoginStatus -> Element Msg
makeCreateOrLogOutButton status =
    let
        (action, buttonText) = 
            case status of
                LoggedIn -> (Logout, "Logout")
                LoggedOut -> (CreateAccountPage, "Sign Up")
                Model.Model.Signup -> (LoginPage, "Login")
                Pending -> (CreateAccountPage, "")

    in
            Input.button
                (List.append buttonAttributes [ height <| px navBarHeight, alignRight ])
                { onPress = Just action
                , label = el [ centerX ] <| text buttonText
                } 



viewLoggedOutRow: Model -> Element Msg
viewLoggedOutRow model =
    let
        promptText = if model.loginStatus == Signup then "Sign Up" else "Login"
        buttonAction = if model.loginStatus == Signup then CreateAccount else Login
    in
    el [ paddingXY 0 100, centerX ] <|
    column (loginCardAttributes model)
        [ el [ centerX, width <| px 250, paddingEach { edges | top = 20 } ] <|
            Input.username
                [ Border.rounded 5, Background.color lightColor, Font.color <| Element.rgb 0 0 0 ]
                { onChange = ChangeUserName 
                , text = model.userName
                , placeholder = Just (Input.placeholder [ Font.color <| Element.rgb 0 0 0 ] (text "Email"))
                , label = Input.labelAbove [] none
                }
        , el [ centerX, width <| px 250, paddingEach { edges | top = 20 } ] <|
            Input.newPassword
                [ Border.rounded 5, Background.color lightColor, Font.color <| Element.rgb 0 0 0 ]
                { onChange = ChangePassword
                , text = model.password
                , placeholder = Just (Input.placeholder [ Font.color <| Element.rgb 0 0 0] (text "Password"))
                , label = Input.labelAbove [] none
                , show = False
                }
        , el [ centerX, width <| px 250, paddingEach { edges | top = 20, bottom = 20 } ] <|
            Input.button
                (centerX :: buttonAttributes)
                { onPress = Just buttonAction
                , label = el [ padding 10, centerX ] <| text promptText
                } 
        ]
