
-------------------------------------------------------------------------------
-- JS to Haskell rules:
-- - if a JS class can be inherited -> define a Haskell typeclass
-- - if a JS class can be instanciated -> define a Haskell newtype
-- 
-- TODO:
-- - handle null value of a property (use a Maybe for the mkProp function?)
-- - constructors with optional parameters or parameter object
-------------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module API 
  ( Object3DC(..)

  , MaterialC(..)
  , MeshLambertMaterial(..)
  , newMeshLambertMaterial

  , LightC(..)
  , PointLight(..)
  , newPointLight

  , CameraC
  , PerspectiveCamera(..)
  , newPerspectiveCamera

  , Vector3(..)
  , newVector3
  , vector3ToXYZ
  , setXYZ
  , z_

  , Mesh(..)
  , newMesh

  , Scene(..)
  , newScene
  , isScene

  , BufferGeometryC(..)
  , BufferGeometry(..)

  , SphereGeometry(..)
  , newSphereGeometry

  , WebGLRenderer(..)
  , newWebGLRenderer
  , render
  , domElement
  , setSize

  , winInnerWidth
  , winInnerHeight
  , appendInBody

  , valToNumber
  , getProp
  , setProp
  , modifyProp
  ) where

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J hiding (getProp, setProp)

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

new' :: MakeArgs a => (JSVal -> b) -> JSString -> a -> JSM b
new' f name args = do
  v <- jsg ("THREE" :: JSString) ! name
  f <$> J.new v args

mkProp :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> a -> (b -> JSM b) -> JSM b
mkProp name v f = do
    x0 <- fromJSValUnchecked =<< v ! name
    x1 <- f x0
    v ^. jss name x1
    pure x1

getProp :: (t1 -> (a -> JSM a) -> t2) -> t1 -> t2
getProp fProp v = fProp v pure

setProp :: (p -> (b -> JSM a1) -> JSM a2) -> a1 -> p -> JSM ()
setProp fProp x v = void $ fProp v (const $ pure x)

modifyProp :: (p -> (b -> JSM a2) -> JSM a2) -> (b -> JSM a2) -> p -> JSM a2
modifyProp fProp f v = fProp v f

-------------------------------------------------------------------------------
-- Object3D
-------------------------------------------------------------------------------

class Object3DC a where
  add :: (Object3DC b, MakeArgs b) => a -> b -> JSM ()
  position :: a -> (Vector3 -> JSM Vector3) -> JSM Vector3

instance Object3DC JSVal where
  add v x = void $ v # ("add" :: JSString) $ x
  position = mkProp "position"

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
  intensity :: a -> (Double -> JSM Double) -> JSM Double

instance LightC JSVal where
  isLight v = fromJSValUnchecked =<< v ! ("isLight" :: JSString)
  intensity = mkProp "intensity"

-------------------------------------------------------------------------------
-- PointLight
-------------------------------------------------------------------------------

newtype PointLight = PointLight { unPointLight :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (LightC)
  deriving Object3DC via JSVal

newPointLight :: JSM PointLight
newPointLight = new' PointLight "PointLight" ()

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
-- Vector3
-------------------------------------------------------------------------------

newtype Vector3 = Vector3 { unVector3 :: JSVal }
  deriving (MakeObject, ToJSVal, MakeArgs)

instance FromJSVal Vector3 where
  fromJSVal = pure .Just . Vector3

newVector3 :: Double -> Double -> Double -> JSM Vector3
newVector3 x y z = new' Vector3 "Vector3" (x, y, z)

z_ :: Vector3 -> (Double -> JSM Double) -> JSM Double
z_ = mkProp "z"

setXYZ :: Double -> Double -> Double -> Vector3 -> JSM ()
setXYZ x y z (Vector3 v) = void $ v ^. js3 "set" x y z

vector3ToXYZ :: Vector3 -> JSM (Double, Double, Double)
vector3ToXYZ (Vector3 v) = do
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

