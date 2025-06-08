{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle as JS
import Miso 
import Miso.String (ms)

import THREE.AmbientLight
import THREE.Light
import THREE.Mesh

-------------------------------------------------------------------------------
-- test app
-------------------------------------------------------------------------------

app :: JSM ()
app = do
  consoleLog "begin"

  Image image1 <- newImage "favicon.ico"
  valToStr image1 >>= consoleLog

  mesh1 <- THREE.Mesh.new
  valToStr (unMesh mesh1) >>= consoleLog

  light1 <- THREE.AmbientLight.new 0 0.5
  valToStr (unAmbientLight light1) >>= consoleLog
  intensity light1 >>= consoleLog . ms
  dispose light1

  light2 <- THREE.AmbientLight.new 0 0.42
  copy light1 light2 >>= intensity >>= consoleLog . ms

  consoleLog "end"

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run app

