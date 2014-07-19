{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module GLSL.QuasiQuoter.Raw where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


import Instances.TH.Lift ()
import qualified Data.ByteString.Char8 as BS



data GLShaderRaw = GLShaderRaw { unRaw :: BS.ByteString }
    deriving ( Show, Eq, Ord )


glslRaw :: QuasiQuoter
glslRaw = QuasiQuoter { quoteExp = glslRawFromString }


glslRawFile :: FilePath -> Q Exp
glslRawFile fp = do
    qAddDependentFile fp
    glslRawFromString =<< qRunIO (readFile fp)


glslRawFromString :: String -> Q Exp
glslRawFromString s = [| GLShaderRaw (BS.pack s) |]
