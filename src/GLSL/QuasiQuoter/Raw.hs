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
import Data.List.NonEmpty( NonEmpty( (:|) ) )
import GLSL.IncludeParser
import Text.Parsec

import System.Directory
import System.FilePath

import Data.Semigroup

data GLShaderRaw = GLShaderRaw { unRaw :: BS.ByteString }
    deriving ( Show, Eq, Ord )

instance Semigroup GLShaderRaw where
    (GLShaderRaw a) <> (GLShaderRaw b) = GLShaderRaw $ a <> "\n" <> b

emptyShaderRaw :: GLShaderRaw
emptyShaderRaw = GLShaderRaw ""

glslRaw :: QuasiQuoter
glslRaw = QuasiQuoter { quoteExp = glslRawFromString }


glslRawFile :: FilePath -> Q Exp
glslRawFile fp = glslRawFromByteString . unRaw =<< loadFile fp
    
    where
    
    loadFile :: FilePath -> Q GLShaderRaw
    loadFile filePath = do 
        qAddDependentFile filePath
        contentBS <- fmap BS.pack $ qRunIO $ readFile filePath
        processIncludes filePath contentBS


    -- | just as simple as possible, no duplicate or include cycle detection
    -- seeks '#include "filename"' and fuse it directly in to the String
    processIncludes :: FilePath -> ByteString -> Q GLShaderRaw
    processIncludes filePath bs = 
        case parse includeParser filePath bs of
            Left err -> error $ "error while parsing includes" ++ show err
            Right linesWithExternals -> do
                withExternals <- mapM ( unpackExternals (dropFileName filePath) ) linesWithExternals
                return $ sconcat (emptyShaderRaw :| withExternals) 

    unpackExternals :: FilePath -> GLSLWithExternals -> Q GLShaderRaw
    unpackExternals _ (GLSLLine bs)              = return $ GLShaderRaw bs 
    unpackExternals basePath (GLSLExternal relativeFile) = do
        filePath <- qRunIO $ canonicalizePath $ basePath </> relativeFile
        loadFile filePath


glslRawFromString :: String -> Q Exp
glslRawFromString = glslRawFromByteString . BS.pack


glslRawFromByteString :: ByteString -> Q Exp
glslRawFromByteString bs = [| GLShaderRaw bs |]
