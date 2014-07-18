module Main where

import ClassyPrelude
import Test.Hspec

import qualified GLSL.QuasiQuoter.Raw as Raw

simpleGLSLVert :: Raw.GLShaderRaw
simpleGLSLVert = [Raw.glslRaw|
#version 410 core

in vec3 vPosition;
in vec2 vTexture;

uniform mat4 ProjMatrix     = mat4(0.0);
uniform mat4 ModelMatrix    = mat4(0.0);

out vec2 VertexUV;
out vec4 VertexPos;

void main()
{
    VertexPos   = ProjMatrix * ModelMatrix * vec4(vPosition, 1.0);
    
    VertexUV    = vTexture;

    gl_Position = VertexPos;
}
|]

main :: IO ()
main = hspec $ do
    describe "simple bytestring QQ" $ do
        it "parses a file and in-code qq" $
            -- append nl because of inline QQ above
            ("\n" ++ Raw.unRaw $(Raw.glslRawFile "test/res/simple.vert")) `shouldBe` Raw.unRaw simpleGLSLVert