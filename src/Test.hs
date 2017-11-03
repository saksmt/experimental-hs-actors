module Test where

import Actor

data MyActorState = MyActorState { invocations :: Int, greeting :: String } deriving Show
data SetGreeting = SetGreeting String deriving Show
data Greet = Greet deriving Show
data Greeting = Greeting String

myActor :: Actor MyActorState ()
myActor = do
    setState $ MyActorState 0 "Hello!"
    receive $ do
        match $ \(SetGreeting newGreeting) -> updateState $ \old -> return old { greeting = newGreeting }
        match $ \Greet -> withSender $ \sender -> withState $ \state -> sender `tell` (Greeting $ greeting state)
