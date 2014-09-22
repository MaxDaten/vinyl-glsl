{-# LANGUAGE TemplateHaskell #-}
import Prelude
import GLSL

import Test.DocTest


-- | >>> shaderName simpleVert
-- "test/res/simple.vert"
--
-- >>> shaderType simpleVert
-- VertexShader
simpleVert :: ShaderSource VertexShader
simpleVert = $("test/res/simple.vert" `shaderFile` VertexShader)


main :: IO ()
main = doctest ["test/Example.hs"]

