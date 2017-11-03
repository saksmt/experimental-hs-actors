module Actor.DSL.Instances where

import Actor.DSL.Def
import Actor.Def.Types

import Control.Applicative ((<|>))

instance Functor (ActorReceiveMatch s) where
    f `fmap` m = m { actuallyIgnored = f $ actuallyIgnored m }

instance Applicative (ActorReceiveMatch s) where
    pure x = ActorReceiveMatch { receivePart = const Nothing, actuallyIgnored = x }
    f <*> m = let (ActorReceiveMatch mReceive mv) = m
                  (ActorReceiveMatch fReceive fv) = f
                  resultReceive d = mReceive d <|> fReceive d
              in ActorReceiveMatch { receivePart = resultReceive, actuallyIgnored = fv mv }

instance Monad (ActorReceiveMatch s) where
    m >>= a = ActorReceiveMatch { actuallyIgnored = value, receivePart = resultReceivePart }
        where (ActorReceiveMatch originalReceive originalValue) = m
              (ActorReceiveMatch newReceive value) = a originalValue
              resultReceivePart d = originalReceive d <|> newReceive d
