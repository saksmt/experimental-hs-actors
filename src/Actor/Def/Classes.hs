{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.Def.Classes where

import Data.Dynamic

class Monad m => ActorActionMonad (m) where
    data ActorPathFor m :: *

    tell :: Typeable msg => (ActorPathFor m) -> msg -> m ()

    sender :: m (ActorPathFor m)

    withSender :: ((ActorPathFor m) -> m ()) -> m ()
    withSender f = sender >>= f

class (Monad m, ActorActionMonad m) => StatefulActor s (m :: * -> *) where
    withState :: (s -> m ()) -> m ()
    withState f = getState >>= f

    updateState :: (s -> m s) -> m ()
    updateState f = do
        old <- getState
        new <- f old
        setState new

    getState :: m s
    setState :: s -> m ()
