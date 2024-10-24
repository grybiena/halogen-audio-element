module Test.Main where

import Prelude

import Effect (Effect)
import Halogen.Audio.Element.Event (AudioElementEvent(..), everything)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)


main :: Effect Unit
main = do
  runSpecAndExitProcess [consoleReporter] do
     describe "all audio element events" do
        it "they're all there" do
           everything `shouldEqual` [ Abort 
                                    , CanPlay 
                                    , CanPlayThrough 
                                    , DurationChange 
                                    , Emptied
                                    , Ended
                                    , Error
                                    , LoadedData
                                    , LoadedMetaData
                                    , LoadStart
                                    , Pause
                                    , Play
                                    , Playing
                                    , Progress
                                    , RateChange
                                    , Seeked
                                    , Seeking
                                    , Stalled
                                    , Suspend
                                    , TimeUpdate
                                    , VolumeChange
                                    , Waiting
                                    ]


