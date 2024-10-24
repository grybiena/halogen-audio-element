module Halogen.Audio.Element.Event where

import Prelude hiding (top)

import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, succ)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr1)


data AudioElementEvent =
    Abort 
  | CanPlay 
  | CanPlayThrough 
  | DurationChange 
  | Emptied
  | Ended
  | Error
  | LoadedData
  | LoadedMetaData
  | LoadStart
  | Pause
  | Play
  | Playing
  | Progress
  | RateChange
  | Seeked
  | Seeking
  | Stalled
  | Suspend
  | TimeUpdate
  | VolumeChange
  | Waiting

derive instance Generic AudioElementEvent _

instance Show AudioElementEvent where
  show = genericShow

instance Eq AudioElementEvent where
  eq = genericEq

instance Ord AudioElementEvent where
  compare = genericCompare

instance Bounded AudioElementEvent where
  top = genericTop
  bottom = genericBottom

instance Enum AudioElementEvent where
  succ = genericSucc
  pred = genericPred

everything :: forall t a. Enum a => Bounded a => Unfoldable t => t a 
everything = (unfoldr1 (\a -> Tuple a (succ a)) bottom) 


