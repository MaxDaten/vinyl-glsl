{-# LANGUAGE TemplateHaskell #-}
module GLSL.QuasiQuoter where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Instances.TH.Lift ()
import qualified Data.ByteString.Lazy.Char8 as BL



glslBS :: QuasiQuoter
glslBS = QuasiQuoter { quoteExp = glslFromString }


glslFromString :: String -> Q Exp
glslFromString s = [| BL.pack s |]
