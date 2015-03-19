module Main where

import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either

import Control.Functor (($>))
import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.Themes.Bootstrap3.InputGroup as BI

foreign import data HTTP :: !

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

foreign import autocomplete
  "function autocomplete(term) {\
  \  return function(k) {\
  \    return function() {\
  \      var xhr = new XMLHttpRequest();\
  \      xhr.onreadystatechange = function(){\
  \        if (xhr.readyState === 4) {\
  \          k(JSON.parse(xhr.responseText))();\
  \        }\
  \      };\
  \      xhr.open('POST', '/autocomplete?term=' + term, true);\
  \      xhr.send();\
  \    };\
  \  };\
  \}" :: forall eff. String -> ([String] -> Eff (http :: HTTP | eff) Unit) -> Eff (http :: HTTP | eff) Unit

newtype Query = Query String

instance showQuery :: Show Query where
  show (Query s) = s

type Entry = Number

data Input = SetQuery Query
           | SetCompletions [String]
           | OpenMore Entry

data Request = Completions Query
             | Search Query

data Result = Result

type Page = Number

type Offset = Number

data State = State Query [Result] Page Offset

update :: State -> Input -> State
update (State query results page offset) input =
  case input of
    SetQuery query' -> State query' results page offset



view :: forall a. State -> H.HTML a (Either Input Request)
view state =
  case state of
    (State query results page offset) ->
      H.div_
      [ H.div (A.classes [B.navbar, B.navbarDefault, B.navbarStaticTop])
        [ H.div (A.classes [B.navbarHeader, B.navbarLeft])
          [ H.a (A.href "/")
            [ H.img (A.classes []
                    <> A.src "/hayoo.png"
                    <> A.alt "Hayoo! logo"
                    )
              []
            ]
          ]
        ]
      , H.div (A.classes [B.navbarCollapse, B.collapse]
              <> A.id_ "hayoo-navbar-collapse")
        [ H.ul (A.classes [B.nav, B.navbarNav, B.navbarLeft])
          [ H.li (A.classes [B.active])
            [ H.form (A.classes [B.navbarForm, B.navbarLeft]
                      <> A.id_ "search"
                      )
              [ H.div (A.classes [B.formGroup])
                [ H.input (A.classes [B.formControl]
                          <> A.placeholder "Search"
                          <> A.name "query"
                          <> A.id_ "hayoo"
                          <> A.type_ "text"
                          <> A.value (show query)
                          <> A.onInput (pure <<< Right <<< Completions <<< Query)
                          )
                  []
                , H.input (A.classes [B.btn, B.btnDefault]
                          <> A.id_ "submit"
                          <> A.type_ "submit"
                          <> A.value "Search"
                          <> A.onclick (\_ -> E.preventDefault $> Right (Search query))
                          )
                  []

                , H.text (show query)

                ]
              ]
            ]
          ]
        ]
      ]

ui :: forall eff a. SF1 Input (H.HTML a (Either Input Request))
ui = view <$> stateful (State (Query "") [] 0 0) update

handleRequest :: forall eff. Handler Request Input (http :: HTTP | eff)
handleRequest (Completions query) k = do
  autocomplete (show query) \completions ->
    k (SetCompletions completions)

handleRequest (Search query) k = do
  k (SetQuery query)

main = do
  Tuple node driver <- runUIEff ui absurd handleRequest
  appendToBody node