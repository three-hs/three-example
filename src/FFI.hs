{-# LANGUAGE OverloadedStrings #-}

module FFI 
  ( appendInBody
  , consoleLog
  , nullObject
  , valToNumber
  , winInnerWidth
  , winInnerHeight

  , coerceDepthTexture
  , coerceVideoTexture

  , isTexture
  , isDepthTexture
  , isVideoTexture
  , unsafeCoerceTexture
  , unsafeCoerceDepthTexture
  ) where

import Control.Monad (void)
import Control.Lens hiding ((#))
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle as J


import THREE.Texture
import THREE.DepthTexture
import THREE.VideoTexture hiding (update)


appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

consoleLog :: String -> JSM ()
consoleLog v = do
  void $ jsg "console" # "log" $ [v]

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

-------------------------------------------------------------------------------

coerceDepthTexture :: (MakeObject a, ToJSVal a) => a -> JSM (Maybe DepthTexture)
coerceDepthTexture v = do
  r <- fromJSVal =<< v ! "isDepthTexture"
  case r of
    Just True -> Just . DepthTexture <$> toJSVal v
    _ -> pure Nothing

coerceVideoTexture :: (MakeObject a, ToJSVal a) => a -> JSM (Maybe VideoTexture)
coerceVideoTexture v = do
  r <- fromJSVal =<< v ! "isVideoTexture"
  case r of
    Just True -> Just . VideoTexture <$> toJSVal v
    _ -> pure Nothing

-------------------------------------------------------------------------------

isTexture :: (MakeObject a) => a -> JSM Bool
isTexture v = fromMaybe False <$> (fromJSVal =<< v ! "isTexture")

isDepthTexture :: (MakeObject a) => a -> JSM Bool
isDepthTexture v = fromMaybe False <$> (fromJSVal =<< v ! "isDepthTexture")

isVideoTexture :: (MakeObject a) => a -> JSM Bool
isVideoTexture v = fromMaybe False <$> (fromJSVal =<< v ! "isVideoTexture")

unsafeCoerceTexture :: (ToJSVal a) => a -> JSM Texture
unsafeCoerceTexture = fmap Texture . toJSVal 

unsafeCoerceDepthTexture :: (ToJSVal a) => a -> JSM DepthTexture
unsafeCoerceDepthTexture = fmap DepthTexture . toJSVal 

-------------------------------------------------------------------------------

