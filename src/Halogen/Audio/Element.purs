module Halogen.Audio.Element where

import Prelude hiding (top)

import CSS (pct, width)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Data.Traversable (traverse, traverse_)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Audio.Element.Event (AudioElementEvent, audioElementEvents)
import Halogen.HTML.CSS (style)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLAudioElement (HTMLAudioElement, fromElement, toEventTarget, toHTMLMediaElement)
import Web.HTML.HTMLMediaElement (currentSrc, currentTime, duration, ended, muted, pause, paused, play, setCurrentTime, setMuted, setSrc, setVolume, src, volume)

data Query a =
    GetAudioElement (HTMLAudioElement -> a)
  | GetSrc (String -> a)
  | SetSrc String a
  | CurrentSrc (String -> a)
  | Play a
  | Pause a
  | Paused (Boolean -> a)
  | GetCurrentTime (Number -> a)
  | SetCurrentTime Number a
  | Duration (Number -> a)
  | GetVolume (Number -> a)
  | SetVolume Number a
  | Muted (Boolean -> a)
  | SetMuted Boolean a
  | Ended (Boolean -> a)

type Input =
  { controls :: Boolean 
  , source :: String 
  }

type State =
  { config :: Input
  }

data Action =
    Initialize
  | Bubble AudioElementEvent

data Output =
    AudioElementEvent AudioElementEvent
  | AudioElement HTMLAudioElement 


component :: forall m . MonadEffect m => H.Component Query Input Output m 
component =
  H.mkComponent
    { initialState: \config -> { config }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction
                                     , handleQuery = handleQuery
                                     , initialize = Just Initialize
                                     }
    }
  where
    handleAction :: Action -> H.HalogenM State Action () Output m Unit 
    handleAction =
      case _ of
        Initialize -> do
          e <- H.getRef (H.RefLabel "audio")
          flip traverse_ (e >>= fromElement) $ \el -> do
            traverse_ (subscribeToEvent el) audioElementEvents 
            H.raise $ AudioElement el
        Bubble e -> H.raise $ AudioElementEvent e 
  
    handleQuery :: forall s a.
                    MonadEffect m
                 => Query a -> H.HalogenM s Action () Output m (Maybe a) 
    handleQuery q = do
      e <- H.getRef (H.RefLabel "audio")
      flip traverse (e >>= fromElement) $ \ae -> do
        let me = toHTMLMediaElement ae 
        case q of
          GetAudioElement f -> pure $ f ae
          GetSrc f -> H.liftEffect $ f <$> src me 
          SetSrc src a -> H.liftEffect $ const a <$> setSrc src me
          CurrentSrc f -> H.liftEffect $ f <$> currentSrc me 
          Play a -> H.liftEffect $ const a <$> play me
          Pause a -> H.liftEffect $ const a <$> pause me
          Paused f -> H.liftEffect $ f <$> paused me
          GetCurrentTime f -> H.liftEffect $ f <$> currentTime me 
          SetCurrentTime t a -> H.liftEffect $ const a <$> setCurrentTime t me
          Duration f -> H.liftEffect $ f <$> duration me
          GetVolume f -> H.liftEffect $ f <$> volume me
          SetVolume t a -> H.liftEffect $ const a <$> setVolume t me
          Muted f -> H.liftEffect $ f <$> muted me
          SetMuted b a -> H.liftEffect $ const a <$> setMuted b me
          Ended f -> H.liftEffect $ f <$> ended me
 
    subscribeToEvent :: forall o s.
                        MonadEffect m
                     => HTMLAudioElement -> AudioElementEvent -> H.HalogenM s Action () o m Unit 
    subscribeToEvent el t = do
      { emitter, listener } <- H.liftEffect $ HS.create
      callback <- H.liftEffect $ eventListener (const $ HS.notify listener t)
      H.liftEffect $ addEventListener (EventType $ toLower $ show t) callback false (toEventTarget el)
      void $ H.subscribe (Bubble <$> emitter)
    
    render :: State -> H.ComponentHTML Action () m 
    render { config } = 
      HH.audio [ HP.controls config.controls 
               , HP.src config.source 
               , HP.ref (H.RefLabel "audio")
               , style do
                   width (pct 100.0)
               ] []
     
