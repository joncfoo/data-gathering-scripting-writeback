import Protolude
import Control.Concurrent (threadDelay)
import System.IO (hClose)

import qualified Network.Fancy as N
import qualified System.IO.Streams as S


main :: IO ()
main = do
  forM_ [5000..5010] (\p -> N.streamServer (serverSpec p) handleConnection)
  N.sleepForever
  where
    serverSpec port =
      N.serverSpec { N.address = N.IP "localhost" port }


handleConnection :: Handle -> N.Address -> IO ()
handleConnection h _ =
  finally (handleConnection' h) (hClose h)


handleConnection' :: Handle -> IO ()
handleConnection' h = do
  (is, os) <- S.handleToStreams h
  intS <- S.mapMaybe (readMaybe . toS) is
  mvar <- newMVar 50
  tId <- forkIO (handleRead intS mvar)
  handle (handleEx tId) (handleWrite os mvar)
  where
    handleEx :: ThreadId -> IOException -> IO ()
    handleEx tId _ = killThread tId


handleRead :: S.InputStream Int -> MVar Int -> IO ()
handleRead is mvar =
  S.read is >>= \case
    Nothing -> pure ()
    Just n  -> swapMVar mvar n
               >> handleRead is mvar


handleWrite :: S.OutputStream ByteString -> MVar Int -> IO ()
handleWrite os mvar = forever $ do
  i <- readMVar mvar
  S.write (Just (show i <> "\n")) os
  S.write (Just "") os
  threadDelay (1000 * 1000)
