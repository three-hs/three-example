
-------------------------------------------------------------------------------
-- minimal API + example 
-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J
import Miso 

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = run $ do

  doc <- jsg "document"
  win <- jsg "window"
  winWidthD <- valToNumber =<< win ^. js "innerWidth"
  winHeightD <- valToNumber =<< win ^. js "innerHeight"
  let winWidth = round winWidthD
      winHeight = round winHeightD

  scene1 <- newScene 

  light1 <- newPointLight
  light1 ^. jss "intensity" 200   -- TODO
  void $ light1 ^. js "position" ^. js3 "set" 8 8 8    -- TODO
  add scene1 light1

  geometry1 <- newSphereGeometry
  material1 <- newMeshLambertMaterial
  mesh1 <- newMesh geometry1 material1
  add scene1 mesh1

  camera1 <- newPerspectiveCamera 70 (winWidthD / winHeightD) 0.1 100
  camera1 ^. js "position" ^. jss "z" 6   -- TODO

  renderer1 <- newWebGLRenderer
  setSize renderer1 winWidth winHeight True
  elt <- renderer1 ! "domElement"
  _ <- doc ^. js "body" ^. js1 "appendChild" elt
  render renderer1 scene1 camera1

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

new' :: MakeArgs a => (JSVal -> b) -> JSString -> a -> JSM b
new' f name args = do
  v <- jsg ("THREE" :: JSString) ! name
  f <$> J.new v args

-------------------------------------------------------------------------------
-- Object3D
-------------------------------------------------------------------------------

class Object3DC object where
  add :: (Object3DC a, MakeArgs a) => object -> a -> JSM ()

instance Object3DC JSVal where
  add v x = void $ v # ("add" :: JSString) $ x

-------------------------------------------------------------------------------
-- Scene
-------------------------------------------------------------------------------

newtype Scene = Scene { unScene :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving Object3DC via JSVal

newScene :: JSM Scene
newScene = new' Scene "Scene" ()

isScene :: Scene -> JSM Bool
isScene v = fromJSValUnchecked =<< v ! ("isScene" :: JSString)

-------------------------------------------------------------------------------
-- Light
-------------------------------------------------------------------------------

class Object3DC a => LightC a where
  isLight :: a -> JSM Bool
  intensity :: a -> JSM Double

instance LightC JSVal where
  isLight v = fromJSValUnchecked =<< v ! ("isLight" :: JSString)
  intensity v = fromJSValUnchecked =<< v ! ("intensity" :: JSString)

-------------------------------------------------------------------------------
-- PointLight
-------------------------------------------------------------------------------

newtype PointLight = PointLight { unPointLight :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (LightC)
  deriving Object3DC via JSVal

newPointLight :: JSM PointLight
newPointLight = new' PointLight "PointLight" ()

distance :: PointLight -> JSM Double
distance v = fromJSValUnchecked =<< v ! ("distance" :: JSString)

-------------------------------------------------------------------------------
-- Material
-------------------------------------------------------------------------------

class MaterialC a where
  isMaterial :: a -> JSM Bool

instance MaterialC JSVal where
  isMaterial v = fromJSValUnchecked =<< v ! ("isMaterial" :: JSString)

-------------------------------------------------------------------------------
-- MeshLambertMaterial
-------------------------------------------------------------------------------

newtype MeshLambertMaterial = MeshLambertMaterial { unMeshLambertMaterial :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype MaterialC

newMeshLambertMaterial :: JSM MeshLambertMaterial
newMeshLambertMaterial = new' MeshLambertMaterial "MeshLambertMaterial" ()

reflectivity :: MeshLambertMaterial -> JSM Double
reflectivity v = fromJSValUnchecked =<< v ! ("reflectivity" :: JSString)

-------------------------------------------------------------------------------
-- BufferGeometry
-------------------------------------------------------------------------------

class BufferGeometryC a where
  isBufferGeometry :: a -> JSM Bool

instance BufferGeometryC JSVal where
  isBufferGeometry v = fromJSValUnchecked =<< v ! ("isBufferGeometry" :: JSString)

newtype BufferGeometry = BufferGeometry { unBufferGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

newBufferGeometry :: JSM BufferGeometry
newBufferGeometry = new' BufferGeometry "BufferGeometry" ()

-------------------------------------------------------------------------------
-- SphereGeometry
-------------------------------------------------------------------------------

newtype SphereGeometry = SphereGeometry { unSphereGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

newSphereGeometry :: JSM SphereGeometry
newSphereGeometry = new' SphereGeometry "SphereGeometry" ()

-------------------------------------------------------------------------------
-- Mesh
-------------------------------------------------------------------------------

newtype Mesh = Mesh { unMesh :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (Object3DC)

newMesh :: (ToJSVal a, ToJSVal b, BufferGeometryC a, MaterialC b) => a -> b -> JSM Mesh
newMesh geometry' material' = new' Mesh "Mesh" (geometry', material')

-------------------------------------------------------------------------------
-- Camera
-------------------------------------------------------------------------------

class Object3DC a => CameraC a where
  isCamera :: a -> JSM Bool

instance CameraC JSVal where
  isCamera v = fromJSValUnchecked =<< v ! ("isCamera" :: JSString)

-------------------------------------------------------------------------------
-- PerspectiveCamera
-------------------------------------------------------------------------------

newtype PerspectiveCamera = PerspectiveCamera { unPerspectiveCamera :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype CameraC
  deriving Object3DC via JSVal

newPerspectiveCamera :: Double -> Double -> Double -> Double -> JSM PerspectiveCamera
newPerspectiveCamera fov' aspect' near' far' = 
  new' PerspectiveCamera "PerspectiveCamera" (fov', aspect', near', far')

-------------------------------------------------------------------------------
-- WebGLRenderer
-------------------------------------------------------------------------------

newtype WebGLRenderer = WebGLRenderer { unWebGLRenderer :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

newWebGLRenderer :: JSM WebGLRenderer
newWebGLRenderer = new' WebGLRenderer "WebGLRenderer" ()

setSize :: WebGLRenderer -> Int -> Int -> Bool -> JSM ()
setSize (WebGLRenderer v) width height updateStyle = void $ v # ("setSize" :: JSString) $ (width, height, updateStyle)

render :: (ToJSVal a, Object3DC a, ToJSVal b, CameraC b) => WebGLRenderer -> a -> b -> JSM ()
render (WebGLRenderer v) object camera = void $ v # ("render" :: JSString) $ (object, camera)

