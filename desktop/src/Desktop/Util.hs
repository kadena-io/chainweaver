module Desktop.Util where

import Control.Monad (forever)
import Control.Concurrent (MVar, forkIO, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex (PerformEvent, TriggerEvent, Event, newTriggerEvent)

-- | Push writes to the given 'MVar' into an 'Event'.
mvarTriggerEvent
  :: (PerformEvent t m, TriggerEvent t m, MonadIO m)
  => MVar a -> m (Event t a)
mvarTriggerEvent mvar = do
  (e, trigger) <- newTriggerEvent
  _ <- liftIO $ forkIO $ forever $ trigger =<< takeMVar mvar
  pure e
