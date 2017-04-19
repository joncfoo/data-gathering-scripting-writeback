import Protolude
import Control.Concurrent (threadDelay)
import Data.Map.Strict (Map)
import Pipes
import Pipes.Concurrent

import qualified Data.Map.Strict as Map
import qualified Network.Simple.TCP as N
import qualified Pipes.ByteString as PB
import qualified Pipes.Text.Encoding as PE
import qualified Pipes.Prelude.Text as PT
import qualified Pipes.Network.TCP as PN
import qualified Pipes.Prelude as P


main :: IO ()
main = do
  (output, input) <- spawn (bounded 10)
  forM_ [5001..5005] (\p -> forkIO $ N.connect "localhost" (show p) (slurp p output))
  runEffect $ fromInput input >-> sink


sink :: MonadIO m => Consumer (Int, Int) m ()
sink = do
  mv <- liftIO $ newMVar Map.empty
  forever $ do
    (k, v) <- await
    m <- liftIO $ modifyMVar mv (\m -> let m' = Map.insert k v m in pure (m', m'))
    if length m /= 5
      then pure ()
      else
      liftIO $ do
        _ <- swapMVar mv Map.empty
        putStr (Map.showTree m)


slurp :: Int -> Output (Int, Int) -> (N.Socket, N.SockAddr) -> IO ()
slurp port output (sock, _) =
  runEffect $
    void (PE.decodeUtf8 (PN.fromSocket sock 4096))
    >-> toInt
    >-> P.map (\i -> (port, i))
    >-> toOutput output


toInt :: Monad m => Pipe Text Int m ()
toInt = forever $
  (readMaybe . toS) <$> await >>= \case
    Nothing -> pure ()
    Just i  -> yield i
