import Protolude
import Pipes
import Pipes.Concurrent
import Scripting.Lua (LuaState, StackValue(..))
import Data.Hashable (Hashable)

import qualified Data.Map.Strict as Map
import qualified Scripting.Lua.Aeson as LA
import qualified Scripting.Lua as Lua
import qualified Network.Simple.TCP as N
import qualified Pipes.Text.Encoding as PE
import qualified Pipes.Network.TCP as PN
import qualified Pipes.Prelude as P


main :: IO ()
main = do
  (output, input) <- spawn (bounded 10)
  forM_ [5001..5005] (\p -> forkIO $ N.connect "localhost" (show p) (slurp p output))
  runEffect $ fromInput input >-> sink


sink :: MonadIO m => Consumer (Text, Int) m ()
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
        ls <- LA.newstate
        Lua.openlibs ls
        Lua.registerhsfunction ls "hello" hello
        Lua.push ls m *> Lua.setglobal ls "vars"
        Lua.loadfile ls "script.lua"
        Lua.call ls 0 0
        Lua.close ls
  where
    hello :: ByteString -> IO ()
    hello = putText . toS


slurp :: Int -> Output (Text, Int) -> (N.Socket, N.SockAddr) -> IO ()
slurp port output (sock, _) =
  runEffect $
    void (PE.decodeUtf8 (PN.fromSocket sock 4096))
    >-> toInt
    >-> P.map (\i -> ("c" <> show (port - 5000), i))
    >-> toOutput output


toInt :: Monad m => Pipe Text Int m ()
toInt = forever $
  (readMaybe . toS) <$> await >>= \case
    Nothing -> pure ()
    Just i  -> yield i


instance (Eq a, Ord a, Hashable a, StackValue a, StackValue b)
      => StackValue (Map a b) where
  push lua h = pushTextHashMap lua h
  peek lua i = fmap Map.fromList <$> getPairs lua i
  valuetype _ = Lua.TTABLE



-- | Try reading the value under the given index as a list of key-value pairs.
getPairs :: (StackValue a, StackValue b)
         => LuaState -> Int -> IO (Maybe [(a, b)])
getPairs lua t = do
  Lua.pushnil lua
  pairs <- sequence <$> remainingPairs
  return pairs
 where
  t' = if t < 0 then t - 1 else t
  remainingPairs = do
    res <- nextPair
    case res of
      Nothing -> return []
      Just a  -> (a:) <$> remainingPairs
  nextPair = do
    hasNext <- Lua.next lua t'
    if hasNext
      then do
        val <- Lua.peek lua (-1)
        key <- Lua.peek lua (-2)
        Lua.pop lua 1 -- removes the value, keeps the key
        return $ Just <$> ((,) <$> key <*> val)
      else
        return Nothing

-- | Push a hashmap unto the stack.
pushTextHashMap :: (StackValue a, StackValue b) => LuaState -> Map a b -> IO ()
pushTextHashMap lua hm = do
    let xs = Map.toList hm
    Lua.createtable lua (length xs + 1) 0
    let addValue (k, v) = Lua.push lua k *> Lua.push lua v *>
                          Lua.rawset lua (-3)
    mapM_ addValue xs
