{-# LANGUAGE TemplateHaskell #-}
module GLSL.QuasiQuoter where

import ClassyPrelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Instances.TH.Lift ()
import qualified Data.ByteString.Lazy.Char8 as BL



glslBS :: QuasiQuoter
glslBS = QuasiQuoter { quoteExp = glslBSFromString }


glslFileBS :: FilePath -> Q Exp
glslFileBS fp = do
    qAddDependentFile (fpToString fp)
    glslBSFromString =<< qRunIO (readFile fp)


glslBSFromString :: String -> Q Exp
glslBSFromString s = [| BL.pack s |]
