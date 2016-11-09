module Graphics.SignaturePad.Halogen.Component where

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Control.Monad.Aff (Aff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Either (either)
import Data.Foreign (toForeign)
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Graphics.SignaturePad (isEmpty, Config, SignaturePad, mkSignaturePad, defaultConfig, fromDataURL, toDataURL, clear, on, off)
import Prelude (type (~>), pure, void, unit, bind, const, not, flip, (<$>), (<<<), (=<<), ($), ($>))

type SignaturePadEffects eff = (dom :: DOM | eff)

type State =
  { element :: Maybe HTMLCanvasElement
  , signaturePad :: Maybe SignaturePad
  , config :: Config
  , enabled :: Boolean
  }

data Query a
  = SetElement (Maybe HTMLCanvasElement) a
  | Init a
  | Clear a
  | Toggle a
  | Load String a
  | Get (Maybe String -> a)
  | IsEmpty (Maybe Boolean -> a)

initialState :: State
initialState = 
  { element: Nothing
  , signaturePad: Nothing
  , config: defaultConfig
  , enabled: false
  }

component
  :: forall eff
  .  H.Component State Query (Aff (SignaturePadEffects eff))
component = H.lifecycleComponent
  { render
  , eval
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }
  where
  render :: State -> H.ComponentHTML Query
  render _ =
    HH.canvas
    [ HP.ref (\elm -> H.action $ SetElement $ (either (const Nothing ) pure <<< runExcept <<< read <<< toForeign) =<< elm) ]

  eval :: Query ~> H.ComponentDSL State Query (Aff (SignaturePadEffects eff))
  eval (SetElement elm next) = H.modify (_ { element = elm}) $> next
  eval (Init next) = do
    elm <- H.gets _.element
    config <- H.gets _.config
    case elm of
      Nothing -> pure unit
      Just el' -> do
        signaturePad <- H.fromEff $ mkSignaturePad el' config
        H.modify (_ { signaturePad = Just signaturePad, enabled = true })
        pure unit
    pure next
  eval (Clear next) = do
    signaturePad <- H.gets _.signaturePad
    maybe (pure unit) (void) $ H.fromEff <<< clear <$> signaturePad
    pure next
  eval (Toggle next) = do
    H.modify \s -> s { enabled = not s.enabled }
    signaturePad <- H.gets _.signaturePad
    enabled <- H.gets _.enabled
    maybe (pure unit) (void) $ H.fromEff <<< (if enabled then on else off) <$> signaturePad
    pure next
  eval (Load signData next) = do
    signaturePad <- H.gets _.signaturePad
    maybe (pure unit) (void) $ H.fromEff <<< flip fromDataURL signData <$> signaturePad
    pure next
  eval (Get getter) = do
    signaturePad <- H.gets _.signaturePad
    url <- sequence $ H.fromEff <<< flip toDataURL "image/png" <$> signaturePad
    pure $ getter url
  eval (IsEmpty getter) = do
    signaturePad <- H.gets _.signaturePad
    res <- sequence $ H.fromEff <<< isEmpty <$> signaturePad
    pure $ getter res
