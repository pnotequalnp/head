module Main (main) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Foldable (for_)
import Data.Version (showVersion)
import Lens.Family (view)
import Options (Options (..), getOptions)
import Paths_head qualified as Paths
import Pipes
import Pipes.ByteString (fromHandle, splitOn, stdin, stdout, take)
import Pipes.Group (FreeT, intercalates, takes)
import System.Exit (die)
import System.IO (IOMode (..), openFile)
import System.IO.Error (isDoesNotExistError, isPermissionError, tryIOError)
import Prelude hiding (take)

main :: IO ()
main = do
  Options {count, delim, files, quiet, verbose, version} <- getOptions

  when version do
    die (showVersion Paths.version)

  let op = case count of
        Left n -> takeBytes n
        Right n -> takeLines n delim
      source = case files of
        [] -> do
          when verbose do
            yield "==> standard input <==\n"
          op stdin
        ["-"] -> do
          when verbose do
            yield "==> standard input <==\n"
          op stdin
        [filepath] -> do
          when verbose do
            yield ("==> " <> pack filepath <> " <==\n")
          lift (openHandle filepath) >>= op
        _ -> for_ files \filepath -> do
          let fp = case filepath of
                "-" -> "standard input"
                _ -> pack filepath
          unless (quiet && not verbose) do
            yield ("==> " <> fp <> " <==\n")
          lift (openHandle filepath) >>= op
          yield "\n"

  runEffect (source >-> stdout)

lines' :: Monad m => ByteString -> Producer ByteString m a -> FreeT (Producer ByteString m) m a
lines' d = view (splitOn d)

unlines' :: Monad m => ByteString -> FreeT (Producer ByteString m) m a -> Producer ByteString m a
unlines' = intercalates . yield

takeLines :: Monad m => Int -> ByteString -> Producer ByteString m () -> Producer ByteString m ()
takeLines n sep s = do
  (unlines' sep . takes n . lines' sep) s
  yield sep

takeBytes :: Monad m => Int -> Producer ByteString m () -> Producer ByteString m ()
takeBytes n = (>-> take n)

openHandle :: MonadIO m => String -> IO (Producer ByteString m ())
openHandle filepath =
  tryIOError (openFile filepath ReadMode) >>= \case
    Right h -> pure (fromHandle h)
    Left e
      | isDoesNotExistError e -> die ("File `" <> filepath <> "` does not exist")
      | isPermissionError e -> die ("Permission denied when opening file `" <> filepath <> "`")
      | otherwise -> die ("Unknown error occurred when opening file `" <> filepath <> "`")
