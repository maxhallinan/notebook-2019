module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Boolean

data Query a 
  = Toggle a
  | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

button :: forall m. H.Component HH.HTML Query Input Message m
button = 
  H.component 
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query  
  render state =
    let
        label = if state then "On" else "Off"
    in

main :: Effect Unit
main = do
  log "🍝"
