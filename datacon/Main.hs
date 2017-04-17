import Protolude
import System.IO (hClose)

import qualified Network.Fancy as N
import qualified System.IO.Streams as S


main :: IO ()
main = do
  h <- N.connectStream (N.IP "localhost" 5000)
  (is, os) <- S.handleToStreams h
  tId <- forkIO (handleWrite is)
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


handleWrite :: S.InputStream ByteString -> IO ()
handleWrite is =
  S.mapMaybe toInt is
  >>= S.map ((<> "\n") . show)
  >>= S.connectTo S.stdout
