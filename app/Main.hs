module Main (main) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Foldable (for_)
import Lens.Family (view)
import Options (Options (..), getOptions)
import Pipes
import Pipes.ByteString (fromHandle, splitOn, stdin, stdout)
import Pipes.Group (FreeT, intercalates, takes)
import System.Exit (die)
import System.IO (IOMode (..), openFile)
import System.IO.Error (isDoesNotExistError, isPermissionError, tryIOError)

main :: IO ()
main = do
  Options {count, delim, files, quiet} <- getOptions

  let source = case files of
        [] -> takeLines count delim stdin
        _ -> for_ files \filepath -> do
          unless quiet do
            yield ("==> " <> pack filepath <> " <==\n")
          file <- lift (openHandle filepath)
          takeLines count delim file
          yield "\n"

  runEffect (source >-> stdout)

lines' :: Monad m => ByteString -> Producer ByteString m a -> FreeT (Producer ByteString m) m a
lines' d = \s -> view (splitOn d) s -- This is eta expanded to help GHC with type inference

unlines' :: Monad m => ByteString -> FreeT (Producer ByteString m) m a -> Producer ByteString m a
unlines' = intercalates . yield

takeLines :: Monad m => Int -> ByteString -> Producer ByteString m () -> Producer ByteString m ()
takeLines n sep s = do
  (unlines' sep . takes n . lines' sep) s
  yield sep

openHandle :: MonadIO m => String -> IO (Producer ByteString m ())
openHandle "-" = pure stdin
openHandle filepath =
  tryIOError (openFile filepath ReadMode) >>= \case
    Right h -> pure (fromHandle h)
    Left e
      | isDoesNotExistError e -> die ("File `" <> filepath <> "` does not exist")
      | isPermissionError e -> die ("Permission denied when opening file `" <> filepath <> "`")
      | otherwise -> die ("Unknown error occurred when opening file `" <> filepath <> "`")
