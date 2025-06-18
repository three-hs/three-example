
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- import Control.Monad
import Control.Lens hiding ((#))
-- import Data.Foldable (traverse_)
-- import Language.Javascript.JSaddle (valToNumber)
import Language.Javascript.JSaddle  hiding (setProp, getProp)
import Miso (consoleLog, run)
import Miso.String (ms)

import Control.Concurrent
import Control.Monad.IO.Class

import API

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- newScene 

  light1 <- newPointLight
  light1 & setProp intensityProp 400
  light1 & getProp positionProp >>= setXYZ 8 8 8
  add scene1 light1

  material1 <- newMeshLambertMaterial
  geometry1 <- newSphereGeometry
  mesh1 <- newMesh geometry1 material1
  mesh1 & getProp positionProp >>= setXYZ (-1) 0 0
  add scene1 mesh1

  texture2 <- newTextureLoader >>= load "miso.png"
  material2 <- newMeshLambertMaterial
  material2 & setProp mapOptProp texture2
  geometry2 <- newBoxGeometry
  mesh2 <- newMesh geometry2 material2
  mesh2 & getProp positionProp >>= setXYZ 1 0 0
  add scene1 mesh2

  camera1 <- newPerspectiveCamera 70 (winWidth / winHeight) 0.1 100
  camera1 & getProp positionProp >>= setProp zProp 6 

  renderer1 <- newWebGLRenderer
  setSize renderer1 winWidthI winHeightI True

  ----------------------------------------------------------------------
  -- TODO
  let animate1 :: JSVal -> JSVal -> [JSVal] -> JSM ()
      animate1 = fun $ \_ _ _ -> do
        consoleLog "foo"
        liftIO $ threadDelay 1_000_000
        mesh2 & getProp rotationProp >>= modifyProp yRotProp (pure . (+0.1))
        render renderer1 scene1 camera1

  animate2 <- toJSVal $ function $ \_ _ _ -> do
        consoleLog "foo"
        liftIO $ threadDelay 1_000_000
        mesh2 & getProp rotationProp >>= modifyProp yRotProp (pure . (+0.1))
        render renderer1 scene1 camera1
        -- time <- valToNumber t
        -- consoleLog $ ms $ show time

  let animate3 :: JSM JSVal
      animate3 = toJSVal $ do
        consoleLog "foo"
        liftIO $ threadDelay 1_000_000
        mesh2 & getProp rotationProp >>= modifyProp yRotProp (pure . (+0.1))
        render renderer1 scene1 camera1
  ----------------------------------------------------------------------

  unWebGLRenderer renderer1 ^. jss "setAnimationLoop" animate1
  domElement renderer1 >>= appendInBody 

  -- liftIO $ threadDelay 1_000_000
  -- render renderer1 scene1 camera1

{-
  -- tests
  light1 & getProp intensityProp >>= valToNumber >>= consoleLog . ms . show
  light1 & getProp positionProp >>= vector3ToXYZ >>= consoleLog . ms . show
  camera1 & getProp positionProp >>= vector3ToXYZ >>= consoleLog . ms . show
  light1 & getProp positionProp >>= getProp zProp >>= valToNumber >>= consoleLog . ms . show
  light1 & modifyProp intensityProp (pure . (*2)) >>= valToNumber >>= consoleLog . ms . show

  -- check compile errors
  -- scene1 & getProp intensityProp >>= valToNumber >>= consoleLog . ms . show
  -- scene1 & setProp intensityProp 200
  -- scene1 & setProp zProp 200
-}

  pure ()

