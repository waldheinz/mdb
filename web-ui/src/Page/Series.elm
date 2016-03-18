
module Page.Series (
    Model, WithSeries, initialModel,
    Action, view, update, onMount
    ) where

import Effects exposing ( Effects )
import Html exposing ( Html )
import Signal exposing ( Address )

type alias Model =
    {
    }

type alias WithSeries a = { a | pageSeriesModel : Model }

initialModel : Model
initialModel =
    {
    }

type Action
    = NoOp

view : Address Action -> WithSeries a -> Html
view aa wm = Html.div [] [ Html.text "series" ]

onMount : WithSeries a -> (WithSeries a, Effects Action)
onMount wm = ( wm, Effects.none )

update : Action -> WithSeries a -> (WithSeries a, Effects Action)
update a wm = (wm, Effects.none)
