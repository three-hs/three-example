{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle as JS
import Miso 
import Miso.String (ms)

-- lights
import THREE.AmbientLight
import THREE.Light

-- materials
import THREE.MeshNormalMaterial
import THREE.MeshPhysicalMaterial
import THREE.Material


testLights :: JSM ()
testLights = do
  ambientLight1 <- THREE.AmbientLight.new 0 0.01
  valToStr (unAmbientLight ambientLight1) >>= consoleLog
  intensity ambientLight1 >>= consoleLog . ms
  dispose ambientLight1
  ambientLight2 <- THREE.AmbientLight.new 0 0.42
  copy ambientLight1 ambientLight2
  intensity ambientLight1 >>= consoleLog . ms

  pointLight1 <- THREE.AmbientLight.new 0 0.03
  valToStr (unAmbientLight pointLight1) >>= consoleLog
  intensity pointLight1 >>= consoleLog . ms

myFunc :: MeshPhysicalMaterial -> JSM (Double, Double) 
myFunc (MeshPhysicalMaterial v) = do
  x <- v ! ("iridescenceThicknessRange" :: JSString) 
  y0 <- fromJSValUnchecked =<< (x # ("at" :: JSString) $ (0::Int)) 
  y1 <- fromJSValUnchecked =<< (x # ("at" :: JSString) $ (1::Int)) 
  pure (y0, y1)

testMaterials :: JSM ()
testMaterials = do
  meshNormalMaterial1 <- THREE.MeshNormalMaterial.new
  blending meshNormalMaterial1 >>= consoleLog . ms . fromEnum
  blending meshNormalMaterial1 >>= consoleLog . ms . show

  meshPhysicalMaterial1 <- THREE.MeshPhysicalMaterial.new
  consoleLog "toto"
  -- myFunc meshPhysicalMaterial1 >>= consoleLog . ms . show
  -- iridescenceThicknessRange meshPhysicalMaterial1 >>= consoleLog . ms . show


main :: IO ()
main = run $ do
  consoleLog "begin"
  testLights
  testMaterials
  consoleLog "end"


#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

