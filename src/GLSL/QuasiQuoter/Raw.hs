{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module GLSL.QuasiQuoter.Raw where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


import Instances.TH.Lift ()

import           Data.ByteString                    ( ByteString )
import qualified Data.ByteString       as BS hiding ( pack )
import qualified Data.ByteString.Char8 as BS        ( pack )

import GLSL.IncludeParser
import Text.Parsec

import System.Directory
import System.FilePath

import Data.Semigroup

data GLShaderRaw = GLShaderRaw { unRaw :: BS.ByteString }
    deriving ( Show, Eq, Ord )

instance Semigroup GLShaderRaw where
    (GLShaderRaw a) <> (GLShaderRaw b) = GLShaderRaw $ a <> "\n" <> b


glslRaw :: QuasiQuoter
glslRaw = QuasiQuoter { quoteExp = glslRawFromString }


glslRawFile :: FilePath -> Q Exp
glslRawFile fp = do
    let basePath = dropFileName fp
    qAddDependentFile fp
    contentBS <- fmap BS.pack $ qRunIO $ readFile fp
    glslRawFromByteString =<< processIncludes basePath contentBS

    where

    -- | just as simple as possible, no duplicate or include cycle detection
    -- seeks '#include "filename"' and fuse it directly in to the String
    processIncludes :: FilePath -> ByteString -> Q ByteString
    processIncludes basePath bs = 
        case parse includeParser fp bs of
            Left err -> error $ "error while parsing includes" ++ show err
            Right linesWithExternals -> fmap (BS.intercalate "\n") $ mapM ( unpackExternals basePath ) linesWithExternals

    unpackExternals :: FilePath -> GLSLWithExternals -> Q ByteString
    unpackExternals _ (GLSLLine bs)              = return bs 
    unpackExternals basePath (GLSLExternal relativeFile) = do
        filePath <- qRunIO $ canonicalizePath $ basePath </> relativeFile
        qAddDependentFile filePath
        fmap BS.pack $ qRunIO $ readFile filePath


glslRawFromString :: String -> Q Exp
glslRawFromString = glslRawFromByteString . BS.pack


glslRawFromByteString :: ByteString -> Q Exp
glslRawFromByteString bs = [| GLShaderRaw bs |]
