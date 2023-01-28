module Hasura exposing (backendTask)

import BackendTask exposing (BackendTask)
import BackendTask.Env
import BackendTask.Http
import FatalError exposing (FatalError)
import Graphql.Document
import Graphql.Operation exposing (RootQuery)
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Encode


backendTask : SelectionSet value RootQuery -> BackendTask FatalError value
backendTask selectionSet =
    BackendTask.Env.expect "ELM_APP_URL"
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\url ->
                BackendTask.Env.expect "ELM_APP_SECRET"
                    |> BackendTask.allowFatal
                    |> BackendTask.andThen
                        (\secret ->
                            BackendTask.Http.request
                                { url = url
                                , method = "POST"
                                , headers =
                                    [ ( "x-hasura-admin-secret", secret )
                                    , ( "Content-Type", "application/json" )
                                    ]
                                , body =
                                    BackendTask.Http.jsonBody
                                        (Json.Encode.object
                                            [ ( "query"
                                              , selectionSet
                                                    |> Graphql.Document.serializeQuery
                                                    |> Json.Encode.string
                                              )
                                            ]
                                        )
                                , retries = Nothing
                                , timeoutInMs = Nothing
                                }
                                (selectionSet
                                    |> Graphql.Document.decoder
                                    |> BackendTask.Http.expectJson
                                )
                                |> BackendTask.allowFatal
                        )
            )
