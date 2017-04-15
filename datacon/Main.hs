import Protolude
import Control.Concurrent (threadDelay)
import System.IO (hClose)

import qualified Network.Fancy as N
import qualified System.IO.Streams as S


main :: IO ()
main = do
  h <- N.connectStream (N.IP "localhost" 5000)
  (is, os) <- S.handleToStreams h
  forkIO (handleWrite is)
  handleRead os


handleRead :: S.OutputStream ByteString -> IO ()
handleRead os = do
  intS <- S.lines S.stdin
          >>= S.mapMaybe toInt
          >>= S.map (\v -> show v <> "\n")
          >>= S.mapM_ (\v -> putText . toS $ "got: " <> v)
  forever $
    S.read intS >>= \case
      Nothing -> mzero
      Just n  -> S.write (Just n) os >> S.write (Just "") os


toInt :: ByteString -> Maybe Int
toInt = readMaybe . toS


handleWrite :: S.InputStream ByteString -> IO ()
handleWrite is =
  S.mapMaybe toInt is
  >>= S.map (\v -> show v <> "\n")
  >>= S.connectTo S.stdout
