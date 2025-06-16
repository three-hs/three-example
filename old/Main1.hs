
-------------------------------------------------------------------------------
-- basic tests, using the current tree github repo
-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Language.Javascript.JSaddle as JS
import Miso 
import Miso.String (ms)

import THREE.AmbientLight
import THREE.Light
import THREE.Material
import THREE.MeshNormalMaterial
import THREE.MeshPhysicalMaterial

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

  consoleLog "begin"


  consoleLog "*** lights ***"

  ambientLight1 <- THREE.AmbientLight.new 0 0.01
  valToStr (unAmbientLight ambientLight1) >>= consoleLog
  intensity ambientLight1 >>= consoleLog . ms
  THREE.Light.dispose ambientLight1
  ambientLight2 <- THREE.AmbientLight.new 0 0.42
  THREE.Light.copy ambientLight1 ambientLight2
  intensity ambientLight1 >>= consoleLog . ms

  pointLight1 <- THREE.AmbientLight.new 0 0.03
  valToStr (unAmbientLight pointLight1) >>= consoleLog
  intensity pointLight1 >>= consoleLog . ms

  consoleLog "*** materials ***"

  meshNormalMaterial1 <- THREE.MeshNormalMaterial.new
  blending meshNormalMaterial1 >>= consoleLog . ms . fromEnum
  blending meshNormalMaterial1 >>= consoleLog . ms . show

  meshPhysicalMaterial1 <- THREE.MeshPhysicalMaterial.new
  iridescenceThicknessRange meshPhysicalMaterial1 >>= consoleLog . ms . show


  consoleLog "end"

