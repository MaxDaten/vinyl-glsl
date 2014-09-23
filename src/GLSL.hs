module GLSL
    ( module Parser
    , module Raw
    , module GLSL
    ) where

import Prelude

import          GLSL.QuasiQuoter.Parser         as Parser
import          GLSL.QuasiQuoter.Raw            as Raw

import          Language.Haskell.TH.Syntax
import          Instances.TH.Lift ()



data VertexShader = VertexShader
    deriving (Show, Ord, Eq)

data FragmentShader = FragmentShader
    deriving (Show, Ord, Eq)


data ShaderSource shTy = ShaderSource
    { shaderName    :: String
    , shaderType    :: shTy
    , shaderRaw     :: String
    } deriving (Show, Ord, Eq)


shaderFile :: Lift tipe => FilePath -> tipe -> Q Exp
shaderFile fp ty = [|ShaderSource fp ty (unRaw $(glslRawFile fp))|]

vertexFile :: FilePath -> Q Exp
vertexFile fp = [| $(shaderFile fp VertexShader) |]

fragmentFile :: FilePath -> Q Exp
fragmentFile fp = [| $(shaderFile fp FragmentShader) |]


instance Lift VertexShader where
    lift VertexShader   = [|VertexShader|] 

instance Lift FragmentShader where
    lift FragmentShader = [|FragmentShader|] 

instance Lift ty => Lift (ShaderSource ty) where
    lift (ShaderSource n t r) = [|ShaderSource n t r|]
