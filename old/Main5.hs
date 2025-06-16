
-------------------------------------------------------------------------------
-- main app (run with `make serve`)
-------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J
import Miso 
import Miso.String (ms)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = run $ do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- newScene 

  light1 <- newPointLight
  light1 ^. setIntensity 200
  void $ light1 ^. getPosition ^. setXYZ 8 8 8    -- TODO
  add scene1 light1

  intensity light1 >>= consoleLog . ms . show
  position light1 >>= toXYZ >>= consoleLog . ms . show
  light1 ^. getIntensity >>= valToNumber >>= consoleLog . ms . show
  light1 ^. getPosition  >>= valToXYZ >>= consoleLog . ms . show

  geometry1 <- newSphereGeometry
  material1 <- newMeshLambertMaterial
  mesh1 <- newMesh geometry1 material1
  add scene1 mesh1

  camera1 <- newPerspectiveCamera 70 (winWidth / winHeight) 0.1 100
  camera1 ^. getPosition ^. setZ 6 
  -- camera1 ^. getPosition ^. jss "z" 6   -- TODO

  renderer1 <- newWebGLRenderer
  setSize renderer1 winWidthI winHeightI True
  domElement renderer1 >>= appendInBody 
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
  position :: object -> JSM Vector3

instance Object3DC JSVal where
  add v x = void $ v # ("add" :: JSString) $ x
  position v = fromJSValUnchecked =<< v ! "position"

getPosition :: (Object3DC a, MakeObject a) => (JSM JSVal -> Const (JSM JSVal) (JSM JSVal)) -> a -> Const (JSM JSVal) a
getPosition = js "position"

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

-- TODO add a "LightC a" constraint
setIntensity :: Double -> forall a. MakeObject a => IndexPreservingGetter a (JSM ())
setIntensity = jss "intensity"

-- TODO add a "LightC a" constraint
getIntensity :: (LightC a, MakeObject a) => (JSM JSVal -> Const (JSM JSVal) (JSM JSVal)) -> a -> Const (JSM JSVal) a
getIntensity = js "intensity"

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

-- the WebGLRenderer constructor creates a canvas element which can be added in the DOM
domElement :: WebGLRenderer -> JSM JSVal
domElement (WebGLRenderer v) = v ! "domElement"

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

newtype Vector3 = Vector3 { unVector3Camera :: JSVal }
  deriving (MakeObject, ToJSVal, MakeArgs)

instance FromJSVal Vector3 where
  fromJSVal = pure .Just . Vector3

newVector3 :: Double -> Double -> Double -> JSM Vector3
newVector3 x y z = new' Vector3 "Vector3" (x, y, z)

-- TODO add a "Vector3 o" contraint
setZ :: Double -> forall o. MakeObject o => IndexPreservingGetter o (JSM ())
setZ = jss "z"
 
setXYZ 
  :: (MakeObject o, Conjoined p, Contravariant f, Functor f) 
  => Double -> Double -> Double -> p (JSM JSVal) (f (JSM JSVal)) -> p o (f o)
setXYZ x y z = js3 "set" x y z

toXYZ :: Vector3 -> JSM (Double, Double, Double)
toXYZ (Vector3 v) = valToXYZ v

valToXYZ :: JSVal -> JSM (Double, Double, Double)
valToXYZ v = do
  x <- fromJSValUnchecked =<< v ! "x"
  y <- fromJSValUnchecked =<< v ! "y"
  z <- fromJSValUnchecked =<< v ! "z"
  pure (x, y, z)

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

