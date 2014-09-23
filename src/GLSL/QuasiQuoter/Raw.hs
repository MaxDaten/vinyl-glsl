{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE OverloadedStrings #-}
module GLSL.QuasiQuoter.Raw where

import Prelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


import Instances.TH.Lift ()

import GLSL.IncludeParser
import Text.Parsec
import Text.Parsec.String

import System.FilePath

import Data.Semigroup

data GLShaderRaw = GLShaderRaw { unRaw :: String }
    deriving ( Show, Eq, Ord )

instance Semigroup GLShaderRaw where
    (GLShaderRaw a) <> (GLShaderRaw b) = GLShaderRaw $ a ++ "\n" ++ b

emptyShaderRaw :: GLShaderRaw
emptyShaderRaw = GLShaderRaw ""

glslRaw :: QuasiQuoter
glslRaw = QuasiQuoter { quoteExp = glslRawFromString }


glslRawFile :: FilePath -> Q Exp
glslRawFile fp = glslRawFromString =<< loadFile fp
    
    where
    
    loadFile :: FilePath -> Q String
    loadFile filePath = do 
        qAddDependentFile filePath
        content <- qRunIO $ readFile filePath
        processIncludes filePath content


    -- | just as simple as possible, no duplicate or include cycle detection
    -- seeks '#include "filename"' and fuse it directly in to the String
    processIncludes :: FilePath -> String -> Q String
    processIncludes filePath bs = 
        case parse includeParser filePath bs of
            Left err -> error $ "error while parsing includes" ++ show err
            Right linesWithExternals -> fmap (unlines) $ mapM (unpackExternals (dropFileName filePath)) linesWithExternals

    unpackExternals :: FilePath -> GLSLWithExternals -> Q String
    unpackExternals _ (GLSLLine bs) = return bs
    unpackExternals basePath (GLSLExternal relativeFile) = do
        let filePath = basePath </> relativeFile
        loadFile filePath

-- | lineCleanup to remove all white spaces
glslRawFromString :: String -> Q Exp
glslRawFromString bs = 
    case (parse lineCleanup "" bs) of
        Left err    -> error $ "error during line cleanup" ++ show err
        Right str   -> [| GLShaderRaw str |]


lineCleanup :: Parser String
lineCleanup = do
    optional (many newline)
    ls <- sepEndBy1 (skipMany blank >> many (noneOf "\n") ) (many1 newline)
    return $ unlines ls
