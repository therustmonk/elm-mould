# Mould client for Elm

This package is a client library to interact with [mould][] server.

[mould]: https://github.com/DenisKolodin/mould

## Usage

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
