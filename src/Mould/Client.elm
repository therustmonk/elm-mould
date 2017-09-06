module Mould.Client exposing (..)

{-| Client for interaction with Mould server.

@docs init, subscriptions, update

@docs Msg, OutMsg, Call, Decoder, Model, Flow, Protocol

-}

import Json.Encode as JS
import Json.Decode as JD exposing (field)
import Mould

{-| Message to interact with client state.
-}
type Msg req inp
    = Interact Mould.Response
    | Request req
    | In inp
    | Cancel

{-| Output message of the client.
-}
type OutMsg item out
    = Item item
    | Out out
    | Broken String

{-| Decoder type for items or single result.
-}
type Decoder item out
    = ItemFlow (JD.Decoder item) out
    | OutOnly (JD.Decoder out)
    | Explicit out

{-| Model of the client.
-}
type alias Model item out =
    { url : String
    , service : String
    , hasTask : Bool
    , decoder : Maybe (Decoder item out)
    , item : Maybe JS.Value
    }

{-| Initialize mould client.
-}
init : String -> String -> Model item out
init url service =
    { url = url
    , service = service
    , hasTask = False
    , decoder = Nothing
    , item = Nothing
    }

{-| Subscribe for mould events.
-}
subscriptions : Model item out -> Sub (Msg req inp)
subscriptions model =
    Mould.interact model.url model.service Interact

{-| Type to call a mould service.
-}
type alias Call item out =
    { action : String
    , payload : JS.Value
    , decoder : Decoder item out
    }

{-| Protocol of the mould service.
-}
type alias Protocol req item out = req -> Call item out

{-| Flow parser of the mould service.
-}
type alias Flow item = item -> JS.Value

{-| Update state of the client.
-}
update
    : Protocol req item out
    -> Flow inp
    -> Msg req inp
    -> Model item out
    -> (Model item out, Maybe (OutMsg item out), Cmd (Msg req inp))
update protocol flowEncoder msg model =
    if not model.hasTask then
        case msg of
            Request req ->
                let
                    call = protocol req
                    request =
                        { service = model.service
                        , action = call.action
                        , payload = call.payload
                    }
                    cmd = Mould.send model.url model.service request
                in
                   ({ model | hasTask = True, item = Nothing, decoder = Just call.decoder }, Nothing, cmd)
            _ ->
                (model, Nothing, Cmd.none)
    else
        case msg of
            Interact Mould.Asking ->
                -- Safe, because will be here if has task only
                let
                    item = model.item
                    model_ = { model | item = Nothing }
                    answer value = (model_, Nothing, Mould.answer model.url model.service value)
                    cancel reason = (model_, Just <| Broken reason, Mould.cancel model.url model.service)
                in
                    case item of
                        Just item ->
                            case model.decoder of
                                Just (ItemFlow itemDecoder _) ->
                                    case JD.decodeValue itemDecoder item of
                                        Ok value -> (model_, Just <| Item value, Cmd.none)
                                        Err reason -> cancel reason
                                Just (OutOnly _) -> answer Nothing
                                Just (Explicit _) -> answer Nothing
                                Nothing -> cancel "item decoder lost"
                        Nothing -> answer Nothing -- to answer first ready
            Interact (Mould.Received object) ->
                -- Store for future decoding
                ({ model | item = Just object }, Nothing, Cmd.none)
            Interact Mould.Done ->
                let
                    item = model.item
                    model_ = { model | item = Nothing, hasTask = False }
                in
                    case (item, model.decoder) of
                        (_, Just (ItemFlow _ out)) ->
                            (model_, Just <| Out out, Cmd.none)
                        (Just item, Just (OutOnly outDecoder)) ->
                            case JD.decodeValue outDecoder item of
                                Ok value -> (model_, Just <| Out value, Cmd.none)
                                Err reason -> (model_, Just <| Broken reason, Cmd.none)
                        (_, Just (Explicit out)) ->
                            (model_, Just <| Out out, Cmd.none)
                        (_, Nothing) ->
                            (model_, Just <| Broken "out decoder lost", Cmd.none)
                        (_, _) ->
                            (model_, Just <| Broken "wrong mould workflow", Cmd.none)
            Interact (Mould.Failed reason) ->
                ({ model | hasTask = False }, Just <| Broken <| Mould.failToString reason, Cmd.none)
            In input ->
                let
                    value = Just <| flowEncoder input
                    cmd = Mould.answer model.url model.service value
                in
                    (model, Nothing, cmd)
            Cancel ->
                let
                    cmd = Mould.cancel model.url model.service
                in
                    (model, Just <| Broken "canceled", cmd)
            _ ->
                (model, Nothing, Cmd.none)

