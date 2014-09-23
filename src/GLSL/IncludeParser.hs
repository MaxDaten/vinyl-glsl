module GLSL.IncludeParser where


import Text.Parsec
import Text.Parsec.ByteString
import Data.ByteString
import qualified Data.ByteString.Char8 as BS


data GLSLWithExternals =
      GLSLLine ByteString
    | GLSLExternal FilePath
    deriving ( Show, Eq, Ord )

type IncludeParser = Parser [GLSLWithExternals]

-- Acts like p and discards any following space character.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  skipMany blank
  return x

blank :: Parser ()
blank = (space >> return ())

glslString :: Parser String
glslString = between (char '\"') (char '\"') $ many1 (noneOf "\"")

include :: Parser GLSLWithExternals
include = fmap GLSLExternal $ lexeme $
    string "#include" >> space >> glslString

-- | parses a code line (inkl. comments) but drops empty lines
line :: Parser GLSLWithExternals
line = fmap (GLSLLine . BS.pack) $ lexeme $ manyTill anyChar ((newline >> return ()) <|> eof)

includeParser :: IncludeParser
includeParser = manyTill (try include <|> line) eof