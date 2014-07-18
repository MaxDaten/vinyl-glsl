{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- https://github.com/elm-lang/Elm/blob/master/compiler/AST/Literal.hs
-- https://github.com/elm-lang/Elm/blob/master/compiler/Parse/Helpers.hs
module GLSL.QuasiQuoter.Parser where

import ClassyPrelude

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Language.GLSL.Parser as GLSLP
import Language.GLSL.Syntax as GLSLS

import Instances.TH.Lift ()



glsl :: QuasiQuoter
glsl = QuasiQuoter { quoteExp = glslFromString }


glslFile :: FilePath -> Q Exp
glslFile fp = do
    qAddDependentFile (fpToString fp)
    glslFromString =<< qRunIO (readFile fp)


glslFromString :: String -> Q Exp
glslFromString s = 
    either (error.show) glslToGLSLShader $ GLSLP.parse s



glslToGLSLShader :: GLSLS.TranslationUnit -> Q Exp -- GLSLShader
glslToGLSLShader = error "glslToGLSLShader undefined"
