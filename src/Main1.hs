{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Language.Javascript.JSaddle as JS
import Miso 
import Miso.String (ms)

import THREE.AmbientLight
import THREE.Light
import THREE.Material
import THREE.MeshNormalMaterial
import THREE.MeshPhysicalMaterial

import THREE.Object3D
import THREE.Scene
import THREE.SphereGeometry
import THREE.MeshLambertMaterial
import THREE.Mesh
import THREE.WebGLRenderer
import THREE.PerspectiveCamera
import THREE.PointLight

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = run $ do
  consoleLog "begin"
  tests
  mwe
  consoleLog "end"

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-------------------------------------------------------------------------------
-- minimal working example
-------------------------------------------------------------------------------

mwe :: JSM ()
mwe = do
  doc <- jsg "document"
  win <- jsg "window"
  winWidthD <- valToNumber =<< win ^. js "innerWidth"
  winHeightD <- valToNumber =<< win ^. js "innerHeight"
  let winWidth = round winWidthD
      winHeight = round winHeightD

  scene1 <- THREE.Scene.new
  geometry1 <- THREE.SphereGeometry.new
  material1 <- THREE.MeshLambertMaterial.new
  mesh1 <- THREE.Mesh.new (toBufferGeometry geometry1) material1
  add scene1 mesh1

  light1 <- THREE.PointLight.new
  light1 ^. jss "intensity" 200
  light1 ^. js "position" ^. js3 "set" 8 8 8
  add scene1 light1

  camera1 <- THREE.PerspectiveCamera.new 70 (winWidthD / winHeightD) 0.1 100
  camera1 ^. js "position" ^. jss "z" 6

  renderer1 <- THREE.WebGLRenderer.new
  setSize renderer1 winWidth winHeight True
  elt <- renderer1 ! "domElement"
  _ <- doc ^. js "body" ^. js1 "appendChild" elt
  render renderer1 scene1 camera1

-------------------------------------------------------------------------------
-- tests
-------------------------------------------------------------------------------

tests :: JSM ()
tests = do

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

