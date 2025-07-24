{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Function ((&))
import Data.Foldable (traverse_)
import Language.Javascript.JSaddle.Runner (run)

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

import THREE.Texture hiding (rotation)
import THREE.DepthTexture

import FFI

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif


-- MeshLambertMaterial
-- map :: Property MeshLambertMaterial (Maybe Texture)
-- map = optional "map"

test :: Three ()
test = do
  texture1 <- unsafeCoerceTexture =<< THREE.DepthTexture.new (32::Int, 32::Int)
  material1 <- THREE.MeshLambertMaterial.new
  material1 & THREE.MeshLambertMaterial.map .= Just texture1

  (Just t1 :: Maybe Texture) <- material1 ^. THREE.MeshLambertMaterial.map
  -- (Just t2 :: Maybe DepthTexture) <- material1 ^. THREE.MeshLambertMaterial.map
  -- (Just t3 :: Maybe VideoTexture) <- material1 ^. THREE.MeshLambertMaterial.map

  consoleLog . show =<< isDepthTexture t1   -- True 
  consoleLog . show =<< isVideoTexture t1   -- False

  mdt <- coerceDepthTexture t1
  case mdt of
    Just depthtexture1 -> consoleLog "texture1 is a DepthTexture"
    Nothing -> consoleLog "texture1 is not a DepthTexture"

  mvt <- coerceVideoTexture t1
  case mvt of
    Just videotexture1 -> consoleLog "texture1 is a VideoTexture"
    Nothing -> consoleLog "texture1 is not a VideoTexture"

  pure ()
  

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- THREE.Scene.new 

  light1 <- THREE.PointLight.new ()
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
  -- geometry2 <- THREE.BoxGeometry.new ()
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new (70, winWidth / winHeight, 0.1, 100)
  camera1 & position !. z .= 6

  renderer1 <- THREE.WebGLRenderer.new Nothing
  renderer1 & setSize (winWidthI, winHeightI, True)

  material1 & onBeforeCompileMaterial (nullObject, renderer1) -- TODO remove

  renderer1 & setAnimationLoop (\_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & rotation !. y .= (time/1000)
    renderer1 & render (scene1, camera1)
    )

  canvas <- renderer1 ^. domElement
  appendInBody canvas

  controls1 <- THREE.OrbitControls.new (camera1, canvas)
  void $ controls1 & update ()


  -----------------------------------------------------------------------------
  -- tests
  -----------------------------------------------------------------------------

  light1 & intensity *= 2
  light1 & intensity %= (*0.5)
  light1 ^. intensity >>= valToNumber >>= consoleLog . show
  light1 ^. position >>= vector3ToXYZ >>= consoleLog . show
  light1 ^. isLight >>= consoleLog . show
  camera1 ^. position >>= vector3ToXYZ >>= consoleLog . show
  light1 ^. position !. z >>= valToNumber >>= consoleLog . show

  light2 <- THREE.PointLight.new ()
  light1 ^. intensity >>= valToNumber >>= consoleLog . show
  light2 ^. intensity >>= valToNumber >>= consoleLog . show
  void $ light2 & copy (light1, True)
  void $ light2 & copy light1
  -- void $ light2 & copy mesh1  -- should not compile
  light1 ^. intensity >>= valToNumber >>= consoleLog . show
  light2 ^. intensity >>= valToNumber >>= consoleLog . show

  test

  pure ()


