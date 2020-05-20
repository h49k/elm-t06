module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


-- MAIN 

main : Program () Model Msg
main =
   Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

-- MODEL


-- 構造体の別名がModel
type alias Model = 
    {
        input: String,
        rdapState: RdapState
    }

-- Model で設定してある RdapState型
-- RDAP問い合わせの状態遷移
type RdapState
    = Init
    | Waiting
    | Loaded Rdap
    | Failed Http.Error

-- Modelを初期化
-- initの値
init : () -> ( Model, Cmd Msg )
init _ =
-- initしたときは、inputは空文字列
    ( Model "" Init
    , Cmd.none )



-- UPDATE
-- メッセージを受け取ったときの処理
type Msg = 
            Input String
            | Send
            | Receive ( Result Http.Error Rdap )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of 
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        -- Send　になったら、rdapStateをWaitingにして Http.Get する。
        Send -> 
            ( { model
                | input = ""
                , rdapState = Waiting
                }
            ,  Http.get
                { url = "https://rdap.db.ripe.net/ip/" ++ model.input
                , expect = Http.expectJson Receive rdapDecoder
                }
            )

        Receive (Ok rdap) -> 
            ( { model | rdapState = Loaded rdap }, Cmd.none )
        
        Receive (Err e) ->
            ( { model | rdapState = Failed e }, Cmd.none )

view : Model -> Html Msg
view model =
    div [ ]
        [ Html.form [ onSubmit Send ]
            [ input
                [ onInput Input
                , autofocus True
                , placeholder "RDAP Query"
                , value model.input
                ]
            []
            , button 
                [ disabled
                (( model.rdapState == Waiting )
                    || String.isEmpty (String.trim model.input )
                )
            ]
            [ text "Query" ]
        ]
        , case model.rdapState of
            Init -> 
                text ""
            
            Waiting ->
                text "Waiting..."

            Loaded rdap ->
                div [] [
                  li [] [ case rdap.startAddress of 
                                Just startAddress ->
                                    text ( "startAddress: " ++ startAddress )
                                Nothing ->
                                    text ""
                                    ]
                , li [] [ case rdap.endAddress of 
                                Just endAddress ->
                                    text ( "endAddress: " ++ endAddress )
                                Nothing ->
                                    text "endAddress: "
                                    ]
                , li [] [ case rdap.country of 
                                Just country ->
                                    text ( "country: " ++ country )
                                Nothing ->
                                    text "country: "
                                    ]
                , li [] [ case rdap.name of
                                Just name ->
                                    text ( "name: " ++ name )
                                Nothing ->
                                    text "name: "
                                    ]
                , li [] [ case rdap.handle of
                                Just handle ->
                                    text ( "handle: " ++ handle )
                                Nothing ->
                                    text "handle:"
                                    ]
                , li [] [ case rdap.port43 of
                                Just port43 ->
                                    text ( "port43: " ++ port43 )
                                Nothing ->
                                    text "No port43"
                                    ]
                                                
                ]

            Failed error ->
                div [] [ text (Debug.toString error) ]
   
    ]



-- DATA

-- 　curl -L https://rdap.db.ripe.net/ip/37.49.226.250
-- {
--   "handle" : "37.49.226.0 - 37.49.226.255",
--   "startAddress" : "37.49.226.0",
--   "endAddress" : "37.49.226.255",
--   "name" : "XEMU-NL-VPS",
--   "country" : "NL",

-- 受け取ったJSONの中で使いそうなフィールドを指定
type alias Rdap = 
    { handle: Maybe String
    , startAddress: Maybe String 
    , endAddress: Maybe String
    , name: Maybe String
    , country: Maybe String
    , port43: Maybe String
    }

-- rdapDecoder : Decoder Rdap
rdapDecoder =
    D.map6 Rdap
        (D.maybe (D.field "handle" D.string))
        (D.maybe (D.field "startAddress" D.string))
        (D.maybe (D.field "endAddress" D.string))
        (D.maybe (D.field "name" D.string))
        (D.maybe (D.field "country" D.string))
        (D.maybe (D.field "port43" D.string))
