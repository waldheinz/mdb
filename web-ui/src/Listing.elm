
module Listing (
    Model, FetchTask, mkModel,
    Action, withFetchTask, update
    ) where

import Effects exposing ( Effects )
import Http
import Task exposing ( Task )

import Types exposing ( .. )

type alias FetchTask a = (Int, Int) -> Task Http.Error (ApiList a)

type alias Model a =
    { items         : List a
    , perPage       : Int
    , currentPage   : Int
    , totalPages    : Maybe Int
    , fetch         : FetchTask a
    }

mkModel : FetchTask a -> Model a
mkModel f =
    { items = []
    , perPage       = 40
    , currentPage   = 0
    , totalPages    = Nothing
    , fetch         = f
    }

type Action a
    = ItemsLoaded (Result Http.Error (ApiList a))

doFetch : Model a -> Effects (Action a)
doFetch m = m.fetch (m.currentPage * m.perPage, m.perPage) |> Task.toResult |> Task.map ItemsLoaded |> Effects.task

withFetchTask : FetchTask a -> Model a -> (Model a, Effects (Action a))
withFetchTask ft m =
    let
        m' = { m | fetch = ft, items = [], currentPage = 0, totalPages = Nothing }
    in
        (m', doFetch m')

updateForResponse : Model a -> ApiList a -> Model a
updateForResponse m al = { m | items = al.items }

update : Action a -> Model a -> (Model a, Effects (Action a))
update a m = case a of
    ItemsLoaded (Err er)    -> Debug.log "error loading list items" |> \_ -> (m, Effects.none)
    ItemsLoaded (Ok al)     -> (updateForResponse m al, Effects.none)
