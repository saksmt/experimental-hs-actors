module Actor.Def.Types where

import Data.Dynamic

data ActorInput s = ActorInput {
    aiInput :: ActorInteraction,
    aiState :: s
} deriving Show

data ActorInteraction = ActorInteraction {
    aiTarget :: ActorPath,
    aiSource :: ActorPath,
    aiMessage :: Dynamic
} deriving Show

data ActorOutput s a = ActorOutput {
    aoOutput :: [ActorInteraction],
    aoState :: s,
    aoResult :: a
} deriving Show

data ActorPath = ActorPath String deriving Show

newtype Actor s a = Actor {
    aReceive :: ActorInput s -> ActorOutput s a
}
