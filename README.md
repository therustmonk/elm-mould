# Mould client for Elm

This package is a client library to interact with [mould][] server.

[mould]: https://github.com/DenisKolodin/mould

## Declare a service

For example, let's declare service of: https://github.com/DenisKolodin/mould-auth

```elm
module MouldAuth exposing (..)

import Json.Encode as JS
import Json.Decode as JD exposing (field)
import Mould.Client as Client

type alias Model = Client.Model () Response

type alias Msg = Client.Msg Request ()

type alias Login = String
type alias Password = String

type Request
    = DoLogin Login Password
    | DoLogout
    | ChangePassword Password

type Response
    = LoginValid Bool
    | LoggedOut
    | PasswordChanged

protocol : Request -> Client.Call () Response
protocol req =
    case req of
        DoLogin login password ->
            { action = "do-login"
            , payload = JS.object
                [ ("login", JS.string login)
                , ("password", JS.string password)
                ]
            , decoder = Client.OutOnly <| JD.map LoginValid (field "success" JD.bool)
            }
        DoLogout ->
            { action = "do-logout"
            , payload = JS.null
            , decoder = Client.Explicit LoggedOut
            }
        ChangePassword password ->
            { action = "change-password"
            , payload = JS.object
                [ ("password", JS.string password)
                ]
            , decoder = Client.Explicit PasswordChanged
            }

flow () = JS.null

update = Client.update protocol flow
```

## Raw usage

```elm

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mould.interact model.url Interact
        , Mould.listen model.url Notification
        ]

type Msg
    = Interact Mould.Response
    | Notification Mould.Notification

taskAuthByTokenId = "token-auth-request"
taskAuthByPasswordId = "credential-auth-request"

taskIds =
    [ taskAuthByTokenId
    , taskAuthByPasswordId
    ]

doAuth : Model -> (Model, Cmd Msg)
doAuth model =
    let
        authCmd =
            if model.connected == True then
                case model.method of
                    Just (ByToken token) ->
                        Mould.send model.url taskAuthByTokenId (authorizeByTokenRequest token)
                    Just (ByPassword login password) ->
                        Mould.send model.url taskAuthByPasswordId (authorizeByPasswordRequest login password)
                    Nothing ->
                        Cmd.none
            else
                Cmd.none
    in
       (model, authCmd)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Notification notif ->
            case notif of
                Mould.ConnectionEstablished ->
                    doAuth { model | connected = True }
                Mould.ConnectionLost ->
                    ({ model | authorized = False, connected = False }, Cmd.none)
                Mould.BeginTask tag ->
                    ({ model | task = Just tag }, Cmd.none)
                Mould.EndTask tag ->
                    ({ model | task = Nothing }, Cmd.none)
                _ ->
                    (model, Cmd.none)
        Interact (Mould.Asking tag) ->
            if List.member tag taskIds then
                (model, Mould.answer model.url tag Nothing)
            else
                (model, Cmd.none)
        Interact (Mould.Done tag) ->
            if List.member tag taskIds then
                ({ model | authorized = True }, Cmd.none)
            else
                (model, Cmd.none)
        Interact response ->
            (model, Cmd.none)
```
