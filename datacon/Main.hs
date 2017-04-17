import Protolude
import Control.Concurrent (threadDelay)
import Data.Map.Strict (Map)
import System.IO (hClose)

import qualified Data.Map.Strict as Map
import qualified Network.Fancy as N
import qualified System.IO.Streams as S


consumer :: MVar (Map Text Text) -> IO ()
consumer cells = forever $
  do
    content <- readMVar cells
    putText (content & Map.showTree & toS)
    threadDelay (1000 * 1000)


main :: IO ()
main = do
  cells <- newMVar Map.empty
  h <- N.connectStream (N.IP "localhost" 5000)
  (is, os) <- S.handleToStreams h
  tId <- forkIO (handleWrite is cells)
  forkIO $ consumer cells
  finally (handleRead os tId) (hClose h)


handleRead :: S.OutputStream ByteString -> ThreadId -> IO ()
handleRead os tId =
  S.lines S.stdin
  >>= S.mapMaybe toInt
  >>= S.map ((<> "\n") . show)
  >>= S.read
  >>= \case
  Nothing -> killThread tId
  Just n  -> S.write (Just n) os
             >> S.write (Just "") os
             >> handleRead os tId


toInt :: ByteString -> Maybe Int
toInt = readMaybe . toS


handleWrite :: S.InputStream ByteString -> MVar (Map Text Text) -> IO ()
handleWrite is cells =
  S.mapMaybe toInt is
  >>= S.map show
  >>= S.mapM fn
  >>= S.connectTo S.stdout
  where
    fn v =
      swapMVar cells (Map.singleton "5000" (toS v))
      >> pure v
