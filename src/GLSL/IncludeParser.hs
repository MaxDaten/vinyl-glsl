module GLSL.IncludeParser where

import Prelude

import Text.Parsec
import Text.Parsec.String


data GLSLWithExternals =
      GLSLLine String
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
line = fmap GLSLLine $ lexeme $ skipMany blank >> manyTill anyChar ((newline >> return ()) <|> eof)

includeParser :: IncludeParser
includeParser = manyTill (try include <|> line) eof