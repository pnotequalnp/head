module Options (Options (..), getOptions) where

import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Options.Applicative
import Options.Applicative.Help (vsep)

data Options = Options
  { count :: !(Either Int Int)
  , delim :: !ByteString
  , quiet :: !Bool
  , verbose :: !Bool
  , version :: !Bool
  , files :: ![FilePath]
  }

getOptions :: IO Options
getOptions = execParser (info (helper <*> parser) (fullDesc <> progDescDoc (Just desc)))
  where
    desc =
      vsep
        [ "Print the first 10 lines of each FILE to standard output.\n"
        , "With more than one FILE, precede each with a header giving the file name."
        , "With no FILE, or when FILE is -, read standard input."
        ]

parser :: Parser Options
parser = Options <$> parseCount <*> parseDelim <*> parseQuiet <*> parseVerbose <*> parseVersion <*> parseFiles

parseLines :: Parser Int
parseLines =
  option auto $
    fold
      [ short 'n'
      , long "lines"
      , value 10
      , metavar "NUM"
      , help "Print the first NUM lines instead of the first 10"
      ]

parseBytes :: Parser Int
parseBytes =
  option auto $
    fold
      [ short 'c'
      , long "bytes"
      , metavar "NUM"
      , help "Print the first NUM bytes of each file"
      ]

parseCount :: Parser (Either Int Int)
parseCount = Left <$> parseBytes <|> Right <$> parseLines

parseDelim :: Parser ByteString
parseDelim = flag "\n" "\0" (short 'z' <> long "zero-terminated" <> help "Line delimiter is NUL, not newline")

parseFiles :: Parser [FilePath]
parseFiles = many (strArgument (metavar "FILE"))

parseQuiet :: Parser Bool
parseQuiet = switch (short 'q' <> long "quiet" <> long "silent" <> help "Never print headers giving file names")

parseVerbose :: Parser Bool
parseVerbose = switch (short 'v' <> long "verbose" <> help "Always print headers giving file names")

parseVersion :: Parser Bool
parseVersion = switch (long "version" <> help "Output version information and exit")
