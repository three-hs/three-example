{-# LANGUAGE OverloadedStrings #-}

module FFI 
  ( appendInBody
  , consoleLog
  , nullObject
  , valToNumber
  , winInnerWidth
  , winInnerHeight

  , isDepthTexture
  , isVideoTexture
  ) where

import Control.Monad (void)
import Control.Lens hiding ((#))
import Data.Maybe (fromMaybe)
import Language.Javascript.JSaddle as J

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

consoleLog :: String -> JSM ()
consoleLog v = do
  void $ jsg "console" # "log" $ [v]

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"



isDepthTexture :: (MakeObject a) => a -> JSM Bool
isDepthTexture v = fromMaybe False <$> (fromJSVal =<< v ! "isDepthTexture")

isVideoTexture :: (MakeObject a) => a -> JSM Bool
isVideoTexture v = fromMaybe False <$> (fromJSVal =<< v ! "isVideoTexture")


