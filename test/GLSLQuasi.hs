{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import qualified GLSL.QuasiQuoter.Raw   as Raw
import qualified GLSL.IncludeParser     as Incl
import qualified Text.Parsec            as P
import qualified Data.ByteString        as BS
import           Control.Monad (when)
import           Data.Either
import           Data.Semigroup


simpleGLSLVert :: Raw.GLShaderRaw
simpleGLSLVert = [Raw.glslRaw|
#version 410 core

#ifndef __SIGNATURE__
#define __SIGNATURE__

in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

out vec2 VertexUV;
out vec4 VertexPos;

#endif // __SIGNATURE__

void main()
{
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vPosition, 1.0);
    
    VertexUV    = vTexture;

    gl_Position = VertexPos;
}
|]


main :: IO ()
main = hspec $ do
    describe "IncludeParser - success cases" $ do
        it "'include' parses a #include line" $
            Incl.include `parse` "#include \"signature.glsl\"" 
                `shouldParse` (Incl.GLSLExternal "signature.glsl")
        
        it "'line' parses a not #include line" $
            Incl.line `parse` "out vec4 VertexPos;" 
                `shouldParse` (Incl.GLSLLine "out vec4 VertexPos;")

        it "'includeParser' parses multiple lines, drops empty lines" $
            Incl.includeParser `parse` "out vec4 VertexPos;\n  \n#include \"signature.glsl\"\nout vec2 VertexUV;" 
                `shouldParse` [ Incl.GLSLLine "out vec4 VertexPos;"
                              , Incl.GLSLExternal "signature.glsl"
                              , Incl.GLSLLine "out vec2 VertexUV;"
                              ]

        it "'includeParser' parses multiple lines, other pragams are untouched" $
            Incl.includeParser `parse` "#version 410 core\nout vec4 VertexPos;" 
                `shouldParse` [ Incl.GLSLLine "#version 410 core"
                              , Incl.GLSLLine "out vec4 VertexPos;"
                              ]

    describe "IncludeParser - failure cases" $ do
        it "fails on empty #include file" $
            Incl.include `shouldFailOn` "#include \"\""


    describe "Semigroup of raw bytestring QQ " $ do
        it "GLShaderRaw is assosiatable" $
            let signatureGLSL :: Raw.GLShaderRaw
                signatureGLSL = [Raw.glslRaw|in vec3 vPosition;|]

                mainGLSL :: Raw.GLShaderRaw
                mainGLSL = [Raw.glslRaw|void main(){ gl_Position = vPosition;}|]

                combinedGLSL :: Raw.GLShaderRaw
                combinedGLSL = 
                    [Raw.glslRaw|
                        in vec3 vPosition;
                        void main(){ gl_Position = vPosition;}
                    |]

            in (signatureGLSL <> mainGLSL) `shouldBe` combinedGLSL

    describe "simple bytestring QQ" $ do
        it "parses a file and in-code qq" $
            -- append nl because of inline QQ above
            ("\n" `BS.append` Raw.unRaw $(Raw.glslRawFile "test/res/simple.vert")) `shouldBe` Raw.unRaw simpleGLSLVert



-- mainly inspired by hspec-attparsec
-- no signatures to keep transformers dependency out
parse parser str = P.parse parser "" str

shouldFailOn parser str =
    parse parser str `shouldSatisfy` isLeft

shouldSucceedOn parser str =
    parse parser str `shouldSatisfy` isRight

shouldParse :: (Show b, Show a, Eq b) =>
                Either a b -> b -> Expectation
shouldParse eitherRes expectedVal = 
    either (expectationFailure . errmsg)
         checkEquality
         eitherRes

    where 
        errmsg err = "  expected: " ++ show expectedVal
                    ++ "\n  but parsing failed with error: " ++ show err

        checkEquality parsedVal =
          when (parsedVal /= expectedVal) $
            expectationFailure $ 
                "  expected: " ++ show expectedVal
                ++ "\n  but got: " ++ show parsedVal 