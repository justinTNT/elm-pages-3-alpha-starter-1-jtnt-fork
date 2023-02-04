module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Dphones.Enum.Order_by
import Dphones.InputObject
import Dphones.Query
import Effect
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Hasura
import Head
import Head.Seo as Seo
import Pages.Msg
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import RouteBuilder exposing (StatefulRoute, StaticPayload)
import Server.Request as Request
import Server.Response as Response exposing (Response)
import SetRenderer
import Shared exposing (..)
import View exposing (View)


type alias Data =
    SetRenderer.SetList


type alias Model =
    Data


type Msg
    = NoOp


type alias RouteParams =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.serverRender
        { head = head
        , data = data
        , action = \_ -> Request.skip "No action."
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init =
                \maybePageUrl sharedModel staticPayload ->
                    ( staticPayload.data, Effect.loaded staticPayload.data )
            , update =
                \pageUrl sharedModel static msg model ->
                    ( model, Effect.none )
            , subscriptions =
                \maybePageUrl routeParams path sharedModel model ->
                    Sub.none
            }


data : RouteParams -> Request.Parser (BackendTask FatalError (Response Data ErrorPage))
data routeParams =
    let
        mixenToBody : Maybe SetRenderer.Set -> List SetRenderer.Mixen -> Data
        mixenToBody setO mixen =
            case setO of
                Nothing ->
                    { set = "Nothing to see, hear", mixen = [] }

                Just set ->
                    { set = set.title, mixen = mixen }

        getSetOrder : Dphones.InputObject.Set_order_byOptionalFields -> Dphones.InputObject.Set_order_byOptionalFields
        getSetOrder args =
            { args | date = Present Dphones.Enum.Order_by.Desc }

        params : Dphones.Query.SetOptionalArguments -> Dphones.Query.SetOptionalArguments
        params args =
            { args
                | order_by = Present [ Dphones.InputObject.buildSet_order_by getSetOrder ]
            }

        getMixOrder : Dphones.InputObject.Mixen_order_byOptionalFields -> Dphones.InputObject.Mixen_order_byOptionalFields
        getMixOrder args =
            { args | index = Present Dphones.Enum.Order_by.Asc }

        getMixWhere tag optionals =
            { optionals
                | list =
                    Dphones.InputObject.buildString_comparison_exp
                        (\compareOptionals ->
                            { compareOptionals
                                | eq_ = Present tag
                            }
                        )
                        |> Present
            }

        mixParams : Maybe SetRenderer.Set -> Dphones.Query.MixenOptionalArguments -> Dphones.Query.MixenOptionalArguments
        mixParams setO args =
            case setO of
                Just set ->
                    { args
                        | order_by = Present [ Dphones.InputObject.buildMixen_order_by getMixOrder ]
                        , where_ = Present (Dphones.InputObject.buildMixen_bool_exp (getMixWhere set.tag))
                    }

                Nothing ->
                    args
    in
    Dphones.Query.set params SetRenderer.setSelection
        |> Hasura.backendTask
        |> BackendTask.andThen
            (\sets ->
                let
                    setO =
                        sets |> List.head
                in
                Dphones.Query.mixen (mixParams setO) SetRenderer.mixenSelection
                    |> Hasura.backendTask
                    |> BackendTask.map (mixenToBody setO >> Response.render)
            )
        |> Request.succeed


head :
    StaticPayload Data ActionData RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Dphones"
        , image =
            { url = Pages.Url.external "https://s3.ap-southeast-2.amazonaws.com/dphon.es/mixtapes/enigma/mix23A.jpg"
            , alt = "Dphones logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "mixen by DJ Dope Inc."
        , locale = Nothing
        , title = static.data.set
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data ActionData RouteParams
    -> View (Pages.Msg.Msg Msg)
view maybeUrl sharedModel model static =
    { title = "DJ Dope Inc. " ++ static.data.set
    , body = [ SetRenderer.view static.data ]
    }
