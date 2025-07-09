{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main where

import Control.Monad (void)
import Data.Function ((&))
import Data.Foldable (traverse_)
import "jsaddle-run" Language.Javascript.JSaddle.Run (run)

import THREE.BoxGeometry
import THREE.Internal
import THREE.Light
import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.OrbitControls
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.SphereGeometry
import THREE.TextureLoader
import THREE.Vector3
import THREE.WebGLRenderer

import FFI

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- THREE.Scene.new 

  light1 <- THREE.PointLight.new
  light1 & intensity .= 300
  light1 ^. position !.. setXYZ 8 8 8
  void $ scene1 & add light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new (geometry1, material1)
  mesh1 & position !. x .= (-1)

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & THREE.MeshLambertMaterial.map .= Just texture2
  geometry2 <- THREE.BoxGeometry.new (1, 1, 1)
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new (70, winWidth / winHeight, 0.1, 100)
  camera1 & position !. z .= 6

  renderer1 <- THREE.WebGLRenderer.new Nothing
  renderer1 & setSize (winWidthI, winHeightI, True)

  renderer1 & setAnimationLoop (\_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & rotation !. y .= (time/1000)
    renderer1 & render (scene1, camera1)
    )

  canvas <- renderer1 ^. domElement
  appendInBody canvas

  controls1 <- THREE.OrbitControls.new (camera1, canvas)
  void $ controls1 & update ()

