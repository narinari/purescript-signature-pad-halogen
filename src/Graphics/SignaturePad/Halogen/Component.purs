module Graphics.SignaturePad.Halogen.Component where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Types (HTMLCanvasElement)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Foreign.Class (read)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Traversable (sequence)
import Data.Void (Void)
import Graphics.SignaturePad (isEmpty, Config, SignaturePad, mkSignaturePad, defaultConfig, fromDataURL, toDataURL, clear, on, off)
import Prelude (type (~>), Unit, bind, const, flip, not, pure, unit, void, ($), ($>), (<#>), (<$>), (<<<), (>>=), (>>>))

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

initialState :: Maybe Config -> State
initialState config =
  { element: Nothing
  , signaturePad: Nothing
  , config: fromMaybe defaultConfig config
  , enabled: false
  }

component
  :: forall m eff
  . ( MonadAff (SignaturePadEffects eff) m
    )
  => Maybe Config
  -> H.Component HH.HTML Query Unit Void m
component config = H.lifecycleComponent
  { initialState: const $ initialState config
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Init)
  , finalizer: Nothing
  }

  where
  render :: State -> H.ComponentHTML Query
  render _ =
    HH.canvas
    [ HP.ref (H.RefLabel "signature") ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (SetElement elm next) = H.modify (_ { element = elm}) $> next
  eval (Init next) = do
    elm <- H.getRef (H.RefLabel "signature") <#> (_ >>= read >>> runExcept >>> either (const Nothing ) Just)
    H.modify _ { element = elm }
    cfg <- H.gets _.config
    for_ elm \el' -> do
        signaturePad <- H.liftEff $ mkSignaturePad el' cfg
        H.modify (_ { signaturePad = Just signaturePad, enabled = true })
        pure unit
    pure next
  eval (Clear next) = do
    signaturePad <- H.gets _.signaturePad
    maybe (pure unit) (void) $ H.liftEff <<< clear <$> signaturePad
    pure next
  eval (Toggle next) = do
    H.modify \s -> s { enabled = not s.enabled }
    signaturePad <- H.gets _.signaturePad
    enabled <- H.gets _.enabled
    maybe (pure unit) (void) $ H.liftEff <<< (if enabled then on else off) <$> signaturePad
    pure next
  eval (Load signData next) = do
    signaturePad <- H.gets _.signaturePad
    maybe (pure unit) (void) $ H.liftEff <<< flip fromDataURL signData <$> signaturePad
    pure next
  eval (Get getter) = do
    signaturePad <- H.gets _.signaturePad
    url <- sequence $ H.liftEff <<< flip toDataURL "image/png" <$> signaturePad
    pure $ getter url
  eval (IsEmpty getter) = do
    signaturePad <- H.gets _.signaturePad
    res <- sequence $ H.liftEff <<< isEmpty <$> signaturePad
    pure $ getter res
