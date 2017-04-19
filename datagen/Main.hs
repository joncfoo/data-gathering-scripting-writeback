import Protolude
import Control.Concurrent (threadDelay)
import Pipes

import qualified Network.Simple.TCP as N
import qualified Pipes.Text.Encoding as PE
import qualified Pipes.Network.TCP as PN
import qualified Pipes.Prelude as P


main :: IO ()
main = do
  forM_ [5001..5005] (\p -> forkIO $ N.serve (N.Host "localhost") (show p) (handleConnection p))
  sleepForever


handleConnection :: Int -> (N.Socket, N.SockAddr) -> IO ()
handleConnection port (sock, sockAddr) = do
  mvar <- newMVar port
  putText ("Listening: " <> show sockAddr)
  _ <- forkIO $ runEffect $
    void (PE.decodeUtf8 (PN.fromSocket sock 4096))
    >-> toInt
    >-> P.mapM_ (toData mvar)
  runEffect $
    fromData mvar
    >-> P.map ((<> "\n") . show)
    >-> PN.toSocket sock
  where
    fromData :: MVar Int -> Producer Int IO ()
    fromData mvar = forever $ do
      lift (sleep $ (port - 5000) * 1000)
      lift (readMVar mvar) >>= yield

    toData :: MVar Int -> Int -> IO ()
    toData mvar i = void $ swapMVar mvar i


-- | sleep for n milliseconds
sleep :: Int -> IO ()
sleep = threadDelay . (* 1000)

-- | Sleep forever. Useful after a server.
sleepForever :: IO ()
sleepForever = threadDelay maxBound >> sleepForever

toInt :: Monad m => Pipe Text Int m ()
toInt = forever $
  (readMaybe . toS) <$> await >>= \case
    Nothing -> pure ()
    Just i  -> yield i
