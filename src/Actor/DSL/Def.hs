module Actor.DSL.Def where

import Actor.Def.Types

import Data.Dynamic

data ActorReceiveMatch s a = ActorReceiveMatch { receivePart :: Dynamic -> Maybe (Actor s ()), actuallyIgnored :: a }
