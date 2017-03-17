module Main where

import Graphics.SignaturePad.Types
import Graphics.SignaturePad.Halogen.Component as SignaturePad
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CSS.Color (rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (maybe, Maybe(..))
import Data.Void (Void, absurd)
import Halogen.Aff (HalogenEffects, runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Prelude (class Eq, class Ord, type (~>), Unit, bind, const, id, pure, unit, ($), (=<<), (==))

type MainEffects = HalogenEffects ()

data Query a
  = SigClear SignaturePadSlot a
  | SigCapture SignaturePadSlot a

type State =
  { sig1Data :: Maybe String
  }

initialState :: State
initialState =
  { sig1Data: Nothing
  }

data SignaturePadSlot = SignaturePadSlot String

derive instance signaturePadSlotOrd :: Ord SignaturePadSlot
derive instance signaturePadSlotEq :: Eq SignaturePadSlot

type MainHTML g = H.ParentHTML Query SignaturePad.Query SignaturePadSlot g
type MainAff = Aff MainEffects
type MainDSL g = H.ParentDSL State Query SignaturePad.Query SignaturePadSlot Void g

ui :: H.Component HH.HTML Query Unit Void MainAff
ui = H.parentComponent { initialState: const initialState, render, eval, receiver: const Nothing }
  where
  padConfig = defaultConfig { backgroundColor = rgb 220 220 220 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
    [ HH.div_
      [ HH.slot
        slotA
        (SignaturePad.component $ Just padConfig)
        unit
        absurd
      , HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick $ HE.input_ $ SigClear slotA
        ]
        [ HH.text "Clear" ]
      , HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick $ HE.input_ $ SigCapture slotA
        ]
        [ HH.text "Capture" ]
      , HH.img
        [ HP.src $ maybe "" id state.sig1Data ]
      ]
    , HH.div_
      [ HH.slot
        slotB
        (SignaturePad.component $ Just padConfig { height = 300.0, width = 500.0 })
        unit
        absurd
      , HH.button
        [ HP.type_ HP.ButtonButton
        , HE.onClick $ HE.input_ $ SigClear slotB
        ]
        [ HH.text "Clear" ]
      ]
    ]

  slotA = SignaturePadSlot "A"
  slotB = SignaturePadSlot "B"

  eval :: Query ~> MainDSL MainAff
  eval (SigClear slot next) = do
    H.query slot $ H.action $ SignaturePad.Clear
    pure next
  eval (SigCapture slot next) = do
    isEmpty <- H.query slot $ H.request SignaturePad.IsEmpty
    sigData <- H.query slot $ H.request SignaturePad.Get
    maybe (pure unit) (if _ then pure unit else H.modify _ { sig1Data = id =<< sigData }) $ id =<< isEmpty

    pure next

main :: Eff MainEffects Unit
main = runHalogenAff $ runUI ui unit =<< awaitBody
