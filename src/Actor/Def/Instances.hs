{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Def.Instances where

import Actor.Def.Types
import Actor.Def.Classes

import Data.Dynamic
import Data.Semigroup ((<>))

instance Functor (Actor s) where
    f `fmap` m = Actor $ \i -> let out = aReceive m i
                                   result = f $ aoResult out
                               in out { aoResult = result }

instance Applicative (Actor s) where
    pure a = Actor $ \i -> ActorOutput { aoOutput = [], aoResult = a, aoState = aiState i }
    f <*> m = Actor $ \i -> let (ActorOutput mout mstate mresult) = aReceive m i
                                (ActorOutput fout fstate fresult) = aReceive f (i { aiState = mstate })
                                result = fresult mresult
                            in ActorOutput {
                                 aoOutput = fout <> mout,
                                 aoResult = result,
                                 aoState = fstate }

instance Monad (Actor s) where
    m >>= a = Actor newReceive
        where oldReceive = aReceive m
              newReceive i = ActorOutput {
                aoOutput = oldOutput ++ newOutput,
                aoState = newState,
                aoResult = newResult
              } where (ActorOutput oldOutput oldState oldResult) = oldReceive i
                      (ActorOutput newOutput newState newResult) = (aReceive $ a oldResult) $ i { aiState = oldState }

instance ActorActionMonad (Actor s) where
    data ActorPathFor (Actor s) = ActorPath

    sender = Actor $ \i -> ActorOutput { aoOutput = [], aoState = aiState i, aoResult = aiSource $ aiInput i }
    tell path message = withSender $ \sender -> withState $ \state -> Actor $ \i -> ActorOutput {
        aoOutput = [ActorInteraction { aiSource = sender, aiTarget = path, aiMessage = toDyn message }],
        aoState = state,
        aoResult = ()
        }

instance StatefulActor s (Actor s) where
    getState = Actor $ \i -> ActorOutput { aoOutput = [], aoState = aiState i, aoResult = aiState i }
    setState state = Actor $ \i -> ActorOutput { aoOutput = [], aoState = state, aoResult = () }
