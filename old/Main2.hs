
-------------------------------------------------------------------------------
-- minimal example using the current tree github repo
-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Lens
import Language.Javascript.JSaddle as JS
import Miso 

import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.SphereGeometry
import THREE.WebGLRenderer

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

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
  _ <- light1 ^. js "position" ^. js3 "set" 8 8 8
  add scene1 light1

  camera1 <- THREE.PerspectiveCamera.new 70 (winWidthD / winHeightD) 0.1 100
  camera1 ^. js "position" ^. jss "z" 6

  renderer1 <- THREE.WebGLRenderer.new
  setSize renderer1 winWidth winHeight True
  elt <- renderer1 ! "domElement"
  _ <- doc ^. js "body" ^. js1 "appendChild" elt
  render renderer1 scene1 camera1

