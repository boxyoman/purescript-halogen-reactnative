module Halogen.ReactNative.Driver  where

import Prelude

import Data.Foldable (foldMap)
import Data.Function.Uncurried (runFn2)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Aff.Driver (HalogenIO)
import Halogen.Aff.Driver as AD
import Halogen.Aff.Driver.State (RenderStateX)
import Halogen.Component (ComponentSlot, Component)
import Halogen.Query.InputF (InputF)
import Halogen.ReactNative.Core (VIEW(..), Native(..), runGraft)
import Halogen.ReactNative.Unsafe.Elements (textElemU, textU)
import ReactNative.Basic (NativeElement, NativeProps, Prop(..), registerComponent)
import ReactNative.Basic as RB


newtype RenderState s (f :: Type -> Type) (g :: Type -> Type) p o =
  RenderState
    { keyId :: Int
    , node :: NativeElement
    , updateView :: NativeElement -> Effect Unit
    }

type AppName = String


runUI
  :: forall f i o
   . Component VIEW f i o Aff
  -> i
  -> AppName
  -> Aff (HalogenIO f o Aff)
runUI component i appName = do
  keyId <- liftEffect (Ref.new 0)
  AD.runUI (mkRenderSpec appName keyId) component i


mkRenderSpec
  :: AppName
  -> Ref Int
  -> AD.RenderSpec VIEW RenderState
mkRenderSpec appName keyRef =
  { render
  , renderChild: identity
  , removeChild: const (pure unit)
  }
  where

  render
    :: forall s f g p o
     . (forall x. InputF x (f x) -> Effect Unit)
    -> (ComponentSlot VIEW g Aff p (f Unit) -> Effect (RenderStateX RenderState))
    -> VIEW (ComponentSlot VIEW g Aff p (f Unit)) (f Unit)
    -> Maybe (RenderState s f g p o)
    -> Effect (RenderState s f g p o)
  render driver child view =
    let node = renderView driver view
     in
      case _ of
        Nothing -> do
          keyId <- Ref.modify (_ + 1) keyRef
          updateView <- registerComponent appName node
          pure $ RenderState { keyId, node : node, updateView }
        Just (RenderState r) -> do
          r.updateView node
          pure $ RenderState { keyId: r.keyId, node : node, updateView: r.updateView }


renderView
  :: forall p i
   . (InputF Unit i -> Effect  Unit)
  -> VIEW p i
  -> NativeElement
renderView driver (VIEW view) = go view
  where
    go :: Native (Array (Prop (InputF Unit i))) p -> NativeElement
    go (Text str) =
      let textElem = textElemU str
          props = runFn2 RB.prop "children" textElem
        in RB.element textU props

    go (Elem nClass props children) =
      let children' = map go children
          childProp = runFn2 RB.prop "children" children'
          nativeProps = (foldMap (renderProp driver) props <> childProp)
       in RB.element nClass nativeProps

    go (Grafted gd) =
      go (runGraft gd)


renderProp
  :: forall i
   . (InputF Unit i -> Effect Unit)
  -> Prop (InputF Unit i)
  -> NativeProps
renderProp driver = case _ of
  Property name value -> runFn2 RB.prop name value
  Handler evType k ->
    runFn2 RB.handlerProp evType (maybe (pure unit) driver <<< k)
  Nested name prop -> runFn2 RB.prop name (renderProp driver prop)
