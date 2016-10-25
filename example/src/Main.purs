module Main where

import Graphics.SignaturePad.Types
import Graphics.SignaturePad.Halogen.Component as SignaturePad
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import CSS.Color (rgb)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor.Coproduct (Coproduct)
import Data.Maybe (maybe, Maybe(..))
import Halogen.Util (runHalogenAff, awaitBody)
import Prelude (id, class Eq, class Ord, type (~>), Unit, unit, pure, bind, (=<<), ($), (==))

type MainEffects = H.HalogenEffects ()

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

type StateP g = H.ParentState State SignaturePad.State Query SignaturePad.Query g SignaturePadSlot
type QueryP = Coproduct Query (H.ChildF SignaturePadSlot SignaturePad.Query)
type MainHTML g = H.ParentHTML SignaturePad.State Query SignaturePad.Query g SignaturePadSlot
type MainAff = Aff MainEffects
type MainDSL g = H.ParentDSL State SignaturePad.State Query SignaturePad.Query g SignaturePadSlot


ui :: H.Component (StateP MainAff) QueryP MainAff
ui = H.parentComponent { render, eval, peek: Nothing }
  where
  padConfig = defaultConfig { backgroundColor = rgb 220 220 220 }
  render :: State -> MainHTML MainAff
  render state =
    HH.div_
    [ HH.div_
      [ HH.slot slotA
          \_ -> { component: SignaturePad.component
                , initialState: SignaturePad.initialState
                    { config = padConfig }
                }
      , HH.button
        [ HP.buttonType HP.ButtonButton
        , HE.onClick $ HE.input_ $ SigClear slotA
        ]
        [ HH.text "Clear" ]
      , HH.button
        [ HP.buttonType HP.ButtonButton
        , HE.onClick $ HE.input_ $ SigCapture slotA
        ]
        [ HH.text "Capture" ]
      , HH.img
        [ HP.src $ maybe "" id state.sig1Data ]
      ]
    , HH.div_
      [ HH.slot slotB
          \_ -> { component: SignaturePad.component
                , initialState: SignaturePad.initialState
                    { config = padConfig { height = 300.0, width = 500.0 }}
                }
      , HH.button
        [ HP.buttonType HP.ButtonButton
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
main = runHalogenAff $ H.runUI ui (H.parentState initialState) =<< awaitBody
