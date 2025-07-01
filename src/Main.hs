{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}

module Main where

import Data.Function ((&))
import Data.Foldable (traverse_)
import Miso (consoleLog, run)
import Miso.String (ms)

import API
import THREE.BoxGeometry
import THREE.Internal
import THREE.Light
import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.SphereGeometry
import THREE.TextureLoader
import THREE.Vector3
import THREE.WebGLRenderer

import Language.Javascript.JSaddle

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

instance FromJSVal Scene where
  fromJSVal = pure . Just . Scene

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
  scene1 & add light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new geometry1 material1
  mesh1 & position !. x .= (-1)

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & THREE.MeshLambertMaterial.map .= Just texture2
  geometry2 <- THREE.BoxGeometry.new
  mesh2 <- THREE.Mesh.new geometry2 material2
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new 70 (winWidth / winHeight) 0.1 100
  camera1 & position !. z .= 6

  renderer1 <- THREE.WebGLRenderer.new
  setSize renderer1 winWidthI winHeightI True

  setAnimationLoop renderer1 $ \_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & rotation !. y .= (time/1000)
    render renderer1 scene1 camera1

  domElement renderer1 >>= appendInBody 

  -----------------------------------------------------------------------------
  -- tests
  -----------------------------------------------------------------------------

  light1 & intensity *= 2
  light1 & intensity %= (*2)
  light1 ^. intensity >>= valToNumber >>= consoleLog . ms . show
  light1 ^. position >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 ^. isLight >>= consoleLog . ms . show
  camera1 ^. position >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 ^. position !. z >>= valToNumber >>= consoleLog . ms . show

  {-
  light2 <- THREE.PointLight.new
  light2 & copy light1
  -- light2 & copy mesh1
  light1 ^. intensity >>= valToNumber >>= consoleLog . ms . show
  light2 ^. intensity >>= valToNumber >>= consoleLog . ms . show
  -}

  pure ()

