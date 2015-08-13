-- From: https://github.com/feuerbach/regex-applicative/wiki/Examples
-- This code demonstrates the use of regex-applicative package.

import Text.Regex.Applicative
import System.Environment
    
data Protocol = HTTP | FTP deriving Show

protocol :: RE Char Protocol
protocol = HTTP <$ string "http" <|> FTP <$ string "ftp"

type Host = String
type Location = String
data URL = URL Protocol Host Location deriving Show

host :: RE Char Host
host = many $ psym $ (/= '/')

url :: RE Char URL
url  = URL <$> protocol <* string "://" <*> host <* sym '/' <*> many anySym

main = do args <- getArgs
          let link = args!!0
          print $ link  =~ url 
