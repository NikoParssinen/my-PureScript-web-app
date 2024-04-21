module Main where

import Prelude

import Data.Number
import Data.Number.Format
import Data.Maybe
import Data.String
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Clicked String | Equals

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = "0"

render state =
  HH.div_
    [ HH.div_ [ HH.text $ state ]
      , HH.div_
          [ HH.button [ HE.onClick \_ -> Clicked "7" ] [ HH.text "7" ]
          , HH.button [ HE.onClick \_ -> Clicked "8" ] [ HH.text "8" ]
          , HH.button [ HE.onClick \_ -> Clicked "9" ] [ HH.text "9" ]
          , HH.button [ HE.onClick \_ -> Clicked "/" ] [ HH.text "/" ]
          ]
      , HH.div_
          [ HH.button [ HE.onClick \_ -> Clicked "4" ] [ HH.text "4" ]
          , HH.button [ HE.onClick \_ -> Clicked "5" ] [ HH.text "5" ]
          , HH.button [ HE.onClick \_ -> Clicked "6" ] [ HH.text "6" ]
          , HH.button [ HE.onClick \_ -> Clicked "*" ] [ HH.text "*" ]
          ]
      , HH.div_
          [ HH.button [ HE.onClick \_ -> Clicked "1" ] [ HH.text "1" ]
          , HH.button [ HE.onClick \_ -> Clicked "2" ] [ HH.text "2" ]
          , HH.button [ HE.onClick \_ -> Clicked "3" ] [ HH.text "3" ]
          , HH.button [ HE.onClick \_ -> Clicked "-" ] [ HH.text "-" ]
          ]
      , HH.div_
          [ HH.button [ HE.onClick \_ -> Clicked "." ] [ HH.text "." ]
          , HH.button [ HE.onClick \_ -> Clicked "0" ] [ HH.text "0" ]
          , HH.button [ HE.onClick \_ -> Equals ] [ HH.text "=" ]
          , HH.button [ HE.onClick \_ -> Clicked "+" ] [ HH.text "+" ]
          ]
    ]

handleAction = case _ of
  Equals      -> H.modify_ \state -> calculate state
  Clicked "+" -> H.modify_ \state -> case calculate state of
                                          state   -> state <> "+"
                                          _       -> calculate state <> "+"
  Clicked "-" -> H.modify_ \state -> case calculate state of
                                          state   -> state <> "-"
                                          _       -> calculate state <> "-"
  Clicked "*" -> H.modify_ \state -> case calculate state of
                                          state   -> state <> "*"
                                          _       -> calculate state <> "*"
  Clicked "/" -> H.modify_ \state -> case calculate state of
                                          state   -> state <> "/"
                                          _       -> calculate state <> "/"
  Clicked "." -> H.modify_ \state -> state <> "."
  Clicked num -> H.modify_ \state -> if state == "0" then num else state <> num
  Clicked _   -> H.modify_ \state -> "0"

calculate :: String -> String
calculate state =
    case split (Pattern "+") state of
        [x,y]   -> toString ((fromMaybe 0.0 (fromString x)) + (fromMaybe 0.0 (fromString y)))
        _       -> case split (Pattern "-") state of
                        [x,y]   -> toString ((fromMaybe 0.0 (fromString x)) - (fromMaybe 0.0 (fromString y)))
                        _       -> case split (Pattern "*") state of
                                        [x,y]   -> toString ((fromMaybe 0.0 (fromString x)) * (fromMaybe 0.0 (fromString y)))
                                        _       -> case split (Pattern "/") state of
                                                        [x,y]   -> toString ((fromMaybe 0.0 (fromString x)) / (fromMaybe 0.0 (fromString y)))
                                                        _       -> state