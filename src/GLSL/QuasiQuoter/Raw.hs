{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module GLSL.QuasiQuoter.Raw where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote


import Instances.TH.Lift ()
import qualified Data.ByteString.Lazy.Char8 as BL



data GLShaderRaw = GLShaderRaw { unRaw :: BL.ByteString }
    deriving ( Show, Eq, Ord )


glslRaw :: QuasiQuoter
glslRaw = QuasiQuoter { quoteExp = glslRawFromString }


glslRawFile :: FilePath -> Q Exp
glslRawFile fp = do
    qAddDependentFile fp
    glslRawFromString =<< qRunIO (readFile fp)


glslRawFromString :: String -> Q Exp
glslRawFromString s = [| GLShaderRaw (BL.pack s) |]
