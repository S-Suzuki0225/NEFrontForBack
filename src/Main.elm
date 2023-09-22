module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, label, p, span, text)
import Html.Attributes exposing (class, classList, disabled, placeholder, value)
import Html.Events exposing (on, onClick, onInput)
import Http
import Json.Decode
import Json.Encode
import Process
import Random
import Task


type Msg
    = SetName String
    | SetPortStr String
    | PreRegister
    | GetPreRegisteredResult (Result Http.Error String)
    | Register
    | GetRegisteredResult (Result Http.Error String)
    | Reset
    | GetRandomResult { preRegisteredResult : Result Http.Error String, registeredResult : Result Http.Error String }
    | NoOp


updateReady : (ReadyType -> ReadyType) -> RegisterStatus -> Maybe RegisterStatus
updateReady updateFn registerStatus =
    case getReadyType registerStatus of
        Just ready ->
            Just <| Ready <| updateFn ready

        _ ->
            Nothing


ifCanUpdate : (a -> Maybe a) -> a -> a
ifCanUpdate fn prev =
    Maybe.withDefault prev <| fn prev


noop : Model -> ( Model, Cmd msg )
noop model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            noop model

        SetName newName ->
            ( { model
                | registerStatus =
                    model.registerStatus
                        |> ifCanUpdate (updateReady (\prev -> { prev | name = newName }))
              }
            , Cmd.none
            )

        SetPortStr newPortStr ->
            ( { model
                | registerStatus =
                    model.registerStatus
                        |> ifCanUpdate (updateReady (\prev -> { prev | portStr = newPortStr }))
              }
            , Cmd.none
            )

        PreRegister ->
            case getReadyType model.registerStatus of
                Just ready ->
                    case createPreRegistering ready of
                        Ok newStatus ->
                            ( { model | registerStatus = PreRegistering newStatus }, preRegister model.preRegisteredResult newStatus.portNum newStatus.name )

                        Err _ ->
                            noop model

                Nothing ->
                    noop model

        GetPreRegisteredResult rtoken ->
            case rtoken of
                Ok token ->
                    ( { model
                        | registerStatus =
                            case model.registerStatus of
                                PreRegistering preRegistering ->
                                    PreRegistered
                                        { name = preRegistering.name
                                        , portNum = preRegistering.portNum
                                        , token = token
                                        }

                                _ ->
                                    model.registerStatus
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | registerStatus =
                            case model.registerStatus of
                                PreRegistering preRegistering ->
                                    PreRegisterFailed
                                        { name = preRegistering.name
                                        , portNum = preRegistering.portNum
                                        , error = Debug.toString error
                                        }

                                _ ->
                                    model.registerStatus
                      }
                    , Cmd.none
                    )

        Register ->
            let
                _ =
                    Debug.log <| Debug.toString model
            in
            ( { model
                | registerStatus =
                    case model.registerStatus of
                        PreRegistered preRegistered ->
                            Registering { name = preRegistered.name, portNum = preRegistered.portNum, token = preRegistered.token }

                        _ ->
                            model.registerStatus
              }
            , case model.registerStatus of
                PreRegistered { token, portNum } ->
                    register model.registeredResult { token = token, portNum = portNum }

                _ ->
                    Cmd.none
            )

        GetRegisteredResult rresult ->
            case rresult of
                Ok result ->
                    ( { model
                        | registerStatus =
                            case model.registerStatus of
                                Registering registering ->
                                    Registered
                                        { name = registering.name
                                        , portNum = registering.portNum
                                        , token = registering.token
                                        , result = result
                                        }

                                _ ->
                                    model.registerStatus
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | registerStatus =
                            case model.registerStatus of
                                Registering registering ->
                                    RegisterFailed
                                        { name = registering.name
                                        , portNum = registering.portNum
                                        , token = registering.token
                                        , error = Debug.toString error
                                        }

                                _ ->
                                    model.registerStatus
                      }
                    , Cmd.none
                    )

        Reset ->
            ( { model
                | registerStatus =
                    reset model.registerStatus
              }
            , Random.generate GetRandomResult randomResultGen
            )

        GetRandomResult randomResult ->
            ( { model | preRegisteredResult = randomResult.preRegisteredResult, registeredResult = randomResult.registeredResult }
            , always Cmd.none <| Debug.log <| Debug.toString randomResult
            )


type alias Model =
    { registerStatus : RegisterStatus
    , preRegisteredResult : Result Http.Error String
    , registeredResult : Result Http.Error String
    }


type alias TextInputLabels =
    { topLeft : Maybe String
    , topRight : Maybe String
    , bottomLeft : Maybe String
    , bottomRight : Maybe String
    }


defaultTextInputLabels : TextInputLabels
defaultTextInputLabels =
    { topLeft = Nothing
    , topRight = Nothing
    , bottomLeft = Nothing
    , bottomRight = Nothing
    }


withTopLeft : String -> TextInputLabels -> TextInputLabels
withTopLeft topLeft labels =
    { labels | topLeft = Just topLeft }


withTopRight : String -> TextInputLabels -> TextInputLabels
withTopRight topRight labels =
    { labels | topRight = Just topRight }


withBottomLeft : String -> TextInputLabels -> TextInputLabels
withBottomLeft bottomLeft labels =
    { labels | bottomLeft = Just bottomLeft }


withBottomRight : String -> TextInputLabels -> TextInputLabels
withBottomRight bottomRight labels =
    { labels | bottomRight = Just bottomRight }


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr ma mb =
    case ma of
        Just a ->
            Just a

        Nothing ->
            mb


textInputView :
    { value : String
    , setValue : String -> msg
    , placeholder : String
    , className : Maybe String
    , labels : TextInputLabels
    , error : Maybe String
    , disabled : Bool
    }
    -> Html msg
textInputView props =
    div
        [ class "form-control"
        , class <| Maybe.withDefault "" props.className
        ]
        [ case maybeOr props.labels.topLeft props.labels.topRight of
            Just _ ->
                label [ class "label" ] <|
                    [ span [ class "label-text" ] [ text <| Maybe.withDefault "" props.labels.topLeft ]
                    , span [ class "label-text-alt" ] [ text <| Maybe.withDefault "" props.labels.topRight ]
                    ]

            Nothing ->
                text ""
        , input
            [ classList
                [ ( "input input-bordered", True )
                , ( "input-error", isJust props.error )
                ]
            , value props.value
            , onInput props.setValue
            , placeholder props.placeholder
            , disabled props.disabled
            ]
            []
        , case props.error of
            Just e ->
                label [ class "label" ]
                    [ span [ class "label-text-alt text-red-400" ] [ text e ]
                    ]

            Nothing ->
                text ""
        , case maybeOr props.labels.bottomLeft props.labels.bottomRight of
            Just _ ->
                label [ class "label" ]
                    [ span [ class "label-text-alt" ] [ text <| Maybe.withDefault "" props.labels.topLeft ]
                    , span [ class "label-text-alt" ] [ text <| Maybe.withDefault "" props.labels.topRight ]
                    ]

            Nothing ->
                text ""
        ]


buttonView : { message : String, onClick : msg, disabled : Bool, className : Maybe String } -> Html msg
buttonView props =
    button
        [ classList
            [ ( "btn btn-outline btn-primary"
              , True
              )
            , case props.className of
                Just c ->
                    ( c, True )

                Nothing ->
                    ( "", False )
            ]
        , onClick props.onClick
        , disabled props.disabled
        ]
        [ text props.message
        ]


init : () -> ( Model, Cmd Msg )
init () =
    ( { registerStatus = Ready { name = "", portStr = "3000" }
      , preRegisteredResult = Ok ""
      , registeredResult = Ok ""
      }
    , Random.generate GetRandomResult randomResultGen
    )


type alias ReadyType =
    { name : String, portStr : String }


type RegisterStatus
    = Ready ReadyType
    | PreRegistering { name : String, portNum : Int }
    | PreRegisterFailed { name : String, portNum : Int, error : String }
    | PreRegistered { name : String, portNum : Int, token : String }
    | Registering { name : String, portNum : Int, token : String }
    | RegisterFailed { name : String, portNum : Int, token : String, error : String }
    | Registered { name : String, portNum : Int, token : String, result : String }


getReadyType : RegisterStatus -> Maybe ReadyType
getReadyType status =
    case status of
        Ready ready ->
            Just ready

        _ ->
            Nothing


createPreRegistering : ReadyType -> Result { portStr : Maybe String } { name : String, portNum : Int }
createPreRegistering ready =
    let
        portNum =
            String.toInt ready.portStr

        name =
            Just ready.name
    in
    case ( portNum, name ) of
        ( Just pn, Just n ) ->
            Ok <| { name = n, portNum = pn }

        _ ->
            Err <|
                { portStr =
                    case portNum of
                        Nothing ->
                            Just "portには数字を入力してください。"

                        _ ->
                            Nothing
                }


reset : RegisterStatus -> RegisterStatus
reset s =
    let
        convertReadyType : { status | name : String, portNum : Int } -> ReadyType
        convertReadyType { name, portNum } =
            { name = name, portStr = String.fromInt portNum }
    in
    case s of
        Ready r ->
            Ready r

        PreRegistering p ->
            Ready <| convertReadyType p

        PreRegisterFailed p ->
            Ready <| convertReadyType p

        PreRegistered p ->
            Ready <| convertReadyType p

        Registering r ->
            Ready <| convertReadyType r

        RegisterFailed r ->
            Ready <| convertReadyType r

        Registered r ->
            Ready <| convertReadyType r


isJust : Maybe a -> Bool
isJust =
    not << isNothing


isNothing : Maybe a -> Bool
isNothing ma =
    case ma of
        Nothing ->
            True

        Just _ ->
            False


readyCardView : Result { props | name : String, portNum : Int } ReadyType -> Html Msg
readyCardView rready =
    let
        portStr =
            case rready of
                Err { portNum } ->
                    String.fromInt portNum

                Ok ready ->
                    ready.portStr

        error =
            case rready of
                Err _ ->
                    Nothing

                Ok ready ->
                    case createPreRegistering ready of
                        Err err ->
                            err.portStr

                        Ok _ ->
                            Nothing

        isReady =
            case rready of
                Err _ ->
                    False

                Ok _ ->
                    True

        name =
            case rready of
                Err otherStatus ->
                    otherStatus.name

                Ok ready ->
                    ready.name
    in
    div [ class "grid grid-flow-row p-8 shadow-md border border-slate-200 rounded-lg" ]
        [ case rready of
            Ok _ ->
                span [ class "text-primary font-bold text-lg mb-8" ] [ text "pre-registerするための名前を入力してください。" ]

            Err _ ->
                text ""
        , span [ class "text-sm" ] [ text "port" ]
        , div
            [ class "grid grid-cols-[auto,auto,auto] grid-flow-col gap-2 items-center justify-start" ]
            [ p [ class "text-sm text-slate-400" ] [ text "localhost:" ]
            , textInputView
                { value = portStr
                , setValue = SetPortStr
                , placeholder = "portを入力してください……✍"
                , className = Just "mr-auto"
                , labels = defaultTextInputLabels
                , error = error
                , disabled = not isReady
                }
            , p [ class "text-sm text-slate-400" ] [ text "/users/pre-register" ]
            ]
        , textInputView
            { value = name
            , setValue = SetName
            , placeholder = "名前を入力してください……✍"
            , className = Just "mt-6"
            , labels = defaultTextInputLabels |> withTopLeft "名前"
            , error = Nothing
            , disabled = not isReady
            }
        , case rready of
            Ok ready ->
                buttonView
                    { message = "送信"
                    , onClick = PreRegister
                    , disabled =
                        isNothing <|
                            Result.toMaybe <|
                                createPreRegistering ready
                    , className = Just "mt-6 btn-wide"
                    }

            Err _ ->
                text ""
        ]


preRegisteredCardView : { props | token : String, portNum : Int } -> Bool -> Html Msg
preRegisteredCardView { token, portNum } isRegisterStarted =
    let
        portStr =
            String.fromInt portNum
    in
    div [ class "p-8 rounded-lg border border-slate-200 shadow-md mt-8 grid grid-flow-row gap-2" ]
        [ span [ class "text-sm text-slate-400" ] [ text "取得したtoken" ]
        , span [ class "font-bold text-lg text-primary" ] [ text token ]
        , div
            [ class "grid grid-cols-[auto,auto,auto] grid-flow-col gap-2 items-center justify-start" ]
            [ p [ class "text-sm text-slate-400" ] [ text "localhost:" ]
            , textInputView
                { value = portStr
                , setValue = always NoOp
                , placeholder = "portを入力してください……✍"
                , className = Just "mr-auto"
                , labels = defaultTextInputLabels
                , error = Nothing
                , disabled = True
                }
            , p [ class "text-sm text-slate-400" ] [ text "/users/register" ]
            ]
        , if isRegisterStarted then
            text ""

          else
            buttonView { message = "登録", onClick = Register, disabled = False, className = Just "mt-4" }
        ]


errorCardView : String -> Html Msg
errorCardView err =
    div [ class "p-8 rounded-lg border border-red-200 bg-red-50 text-red-600 shadow-md mt-8 grid grid-flow-row gap-2" ]
        [ span [ class "text-md" ] [ text err ]
        , buttonView { message = "最初から", onClick = Reset, disabled = False, className = Just "mt-4" }
        ]


view : Model -> Html Msg
view model =
    case model.registerStatus of
        Ready ready ->
            div [ class "m-12" ]
                [ readyCardView <| Ok ready
                ]

        PreRegistering preRegistering ->
            div [ class "m-12" ]
                [ readyCardView <|
                    Err preRegistering
                , div
                    [ class "grid grid-flow-row gap-6 mt-8" ]
                    [ div [ class "loading loading-spinner loading-lg" ] []
                    ]
                ]

        PreRegistered preRegistered ->
            div [ class "m-12" ]
                [ readyCardView <| Err preRegistered
                , preRegisteredCardView preRegistered False
                ]

        PreRegisterFailed preRegisterFailed ->
            div [ class "m-12" ]
                [ readyCardView <| Err preRegisterFailed
                , errorCardView preRegisterFailed.error
                ]

        Registering registering ->
            div [ class "m-12" ]
                [ readyCardView <| Err registering
                , preRegisteredCardView registering True
                , div [ class "mt-6" ] [ div [ class "loading loading-lg loading-spinner" ] [] ]
                ]

        Registered registered ->
            div [ class "m-12" ]
                [ readyCardView <| Err registered
                , preRegisteredCardView registered True
                , div [ class "p-8 rounded-lg border border-slate-200 shadow-md mt-8 grid grid-flow-row gap-2" ]
                    [ span [ class "text-sm text-slate-400" ] [ text "登録結果" ]
                    , span [ class "font-bold text-lg text-primary" ] [ text registered.result ]
                    , buttonView { message = "最初から", onClick = Reset, disabled = False, className = Just "mt-4" }
                    ]
                ]

        RegisterFailed registerFailed ->
            div [ class "m-12" ]
                [ readyCardView <| Err registerFailed
                , preRegisteredCardView registerFailed True
                , errorCardView registerFailed.error
                ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


preRegisteredResultGen : Random.Generator (Result Http.Error String)
preRegisteredResultGen =
    Random.uniform (Ok "01HAPC41BTJRCS24YFRR925R39")
        [ Err <| Http.BadBody "何かおかしいです"
        ]


registeredResultGen : Random.Generator (Result Http.Error String)
registeredResultGen =
    Random.uniform (Ok "<最終的な登録結果>:")
        [ Err <| Http.BadBody "何かおかしいです"
        ]


randomResultGen : Random.Generator { preRegisteredResult : Result Http.Error String, registeredResult : Result Http.Error String }
randomResultGen =
    preRegisteredResultGen
        |> Random.andThen
            (\p ->
                registeredResultGen
                    |> Random.andThen
                        (\r ->
                            Random.constant { preRegisteredResult = p, registeredResult = r }
                        )
            )


preRegister : Result Http.Error String -> Int -> String -> Cmd Msg
preRegister r portNum name =
    Http.post
        { url =
            String.concat
                [ "http://localhost:"
                , String.fromInt portNum
                , "/users/pre-register"
                ]
        , body = Http.jsonBody <| Json.Encode.object [ ( "name", Json.Encode.string name ) ]
        , expect = Http.expectJson GetPreRegisteredResult <| Json.Decode.field "registeration_token" Json.Decode.string
        }


register : Result Http.Error String -> { token : String, portNum : Int } -> Cmd Msg
register r { token, portNum } =
    Http.post
        { url =
            String.concat
                [ "http://localhost:"
                , String.fromInt portNum
                , "/users/register"
                ]
        , body = Http.jsonBody <| Json.Encode.object [ ( "registeration_token", Json.Encode.string token ) ]
        , expect = Http.expectString GetRegisteredResult
        }
