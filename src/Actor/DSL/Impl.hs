module Actor.DSL.Impl where

import Actor.DSL.Def
import Actor.DSL.Instances
import Actor.Def.Types

match f = ActorReceiveMatch { actuallyIgnored = (), receivePart = r }
    where r d = f <$> fromDynamic d

receive :: (ActorReceiveMatch s ()) -> Actor s ()
receive (ActorReceiveMatch { receivePart = dsl }) = Actor $ \i -> let msg = aiMessage $ aiInput i
                                                                  in aReceive (maybe (error "DEADLETTERS") id $ dsl msg) $ i
