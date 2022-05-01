module Options (Options (..), getOptions) where

import Data.ByteString (ByteString)
import Options.Applicative

data Options = Options
  { count :: !Int
  , delim :: !ByteString
  , files :: ![FilePath]
  , quiet :: !Bool
  }

getOptions :: IO Options
getOptions = execParser (info (helper <*> parser) fullDesc)

parser :: Parser Options
parser = Options <$> parseCount <*> parseDelim <*> parseFiles <*> parseQuiet

parseCount :: Parser Int
parseCount = option auto (short 'n' <> long "lines" <> value 10 <> metavar "NUM")

parseDelim :: Parser ByteString
parseDelim = flag "\n" "\0" (short 'z' <> long "zero-terminated")

parseFiles :: Parser [FilePath]
parseFiles = many (strArgument (metavar "FILE"))

parseQuiet :: Parser Bool
parseQuiet = switch (short 'q' <> long "quiet" <> long "silent")
