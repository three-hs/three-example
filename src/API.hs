
-------------------------------------------------------------------------------
-- JS to Haskell rules:
-- - if a JS class can be inherited -> define a Haskell typeclass
-- - if a JS class can be instanciated -> define a Haskell newtype
--
-- properties:
-- - Prop / mkProp for mandatory properties
-- - Prop' / mkProp' for optional properties
-- 
-- TODO:
-- - read-only properties?
-- - constructors with optional parameters or parameter object
-------------------------------------------------------------------------------

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module API 
  ( Object3DC(..)

  , TextureLoader(..)
  , newTextureLoader
  , load

  , Texture(..)

  , MaterialC(..)
  , MeshLambertMaterial(..)
  , newMeshLambertMaterial
  , textureMap'

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
  , y_
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

  , BoxGeometry(..)
  , newBoxGeometry

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
  -- , modifyProp
  ) where

import Control.Monad
import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as J hiding (getProp, setProp)

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

new' :: MakeArgs a => (JSVal -> b) -> JSString -> a -> JSM b
new' f name args = do
  v <- jsg ("THREE" :: JSString) ! name
  f <$> J.new v args

getProp :: (a -> Maybe b -> JSM c) -> a -> JSM c
getProp fProp v = fProp v Nothing

setProp :: (a -> Maybe b -> JSM c) -> b -> a -> JSM ()
setProp fProp x v = void $ fProp v (Just x)

-- property
type Prop a b = a -> Maybe b -> JSM b

mkProp :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> Prop a b
mkProp name v mx1 = do
  x0 <- fromJSValUnchecked =<< v ! name
  case mx1 of
    Nothing -> pure x0
    Just x1 -> v ^. jss name x1 >> pure x1

-- optional property
type Prop' a b = a -> Maybe b -> JSM (Maybe b)

mkProp' :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> Prop' a b
mkProp' name v mx1 = do
  mx0 <- fromJSVal =<< v ! name
  case mx1 of
    Nothing -> pure mx0
    Just x1 -> v ^. jss name x1 >> pure Nothing


{-
mkProp :: (MakeObject a, FromJSVal b, ToJSVal b) => JSString -> a -> (b -> JSM b) -> JSM b
mkProp name v f = do
    x0 <- fromJSValUnchecked =<< v ! name
    x1 <- f x0
    v ^. jss name x1
    pure x1

getProp :: (a -> (b -> JSM b) -> JSM b) -> a -> JSM b
getProp fProp v = fProp v pure

setProp :: (a -> (b -> JSM b) -> JSM b) -> b -> a -> JSM ()
setProp fProp x v = void $ fProp v (const $ pure x)

modifyProp :: (a -> (b -> JSM b) -> JSM b) -> (b -> JSM b) -> a -> JSM b
modifyProp fProp f v = fProp v f
-}

-------------------------------------------------------------------------------
-- Object3D
-------------------------------------------------------------------------------

class Object3DC a where
  add :: (Object3DC b, MakeArgs b) => a -> b -> JSM ()
  position :: Prop a Vector3
  -- position' :: Prop' a Vector3

instance Object3DC JSVal where
  add v x = void $ v # ("add" :: JSString) $ x
  position = mkProp "position"
  -- position' = mkProp' "position"

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
  intensity :: Prop a Double
  -- intensity' :: Prop' a Double

instance LightC JSVal where
  isLight v = fromJSValUnchecked =<< v ! ("isLight" :: JSString)
  intensity = mkProp "intensity"
  -- intensity' = mkProp' "intensity"

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

textureMap' :: Prop' MeshLambertMaterial Texture
textureMap' = mkProp' "map"

-------------------------------------------------------------------------------
-- Texture
-------------------------------------------------------------------------------

newtype Texture = Texture { unTexture :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

instance FromJSVal Texture where
  fromJSVal = pure .Just . Texture

-------------------------------------------------------------------------------
-- TextureLoader
-------------------------------------------------------------------------------

newtype TextureLoader = TextureLoader { unTextureLoader :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 

newTextureLoader :: JSM TextureLoader
newTextureLoader = new' TextureLoader "TextureLoader" ()

load :: JSString -> TextureLoader -> JSM Texture
load url (TextureLoader v) = Texture <$> (v # ("load" :: JSString) $ [url])

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
-- BoxGeometry
-------------------------------------------------------------------------------

newtype BoxGeometry = BoxGeometry { unBoxGeometry :: JSVal }
  deriving (MakeArgs, MakeObject, ToJSVal) 
  deriving newtype (BufferGeometryC)

newBoxGeometry :: JSM BoxGeometry
newBoxGeometry = new' BoxGeometry "BoxGeometry" ()

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

y_ :: Prop Vector3 Double
y_ = mkProp "y"

z_ :: Prop Vector3 Double
z_ = mkProp "z"

setXYZ :: Double -> Double -> Double -> Vector3 -> JSM ()
setXYZ x y z (Vector3 v) = void $ v ^. js3 "set" x y z

vector3ToXYZ :: Vector3 -> JSM (Double, Double, Double)
vector3ToXYZ (Vector3 v) = do
  x <- fromJSValUnchecked =<< v ! "x"
  y <- fromJSValUnchecked =<< v ! "y"
  z <- fromJSValUnchecked =<< v ! "z"
  pure (x, y, z)

