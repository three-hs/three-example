{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle as JS
import Miso 
import Miso.String (ms)

import THREE.AmbientLight
import THREE.Light


testLights :: JSM ()
testLights = do
  ambientLight1 <- THREE.AmbientLight.new 0 0.01
  valToStr (unAmbientLight ambientLight1) >>= consoleLog
  intensity ambientLight1 >>= consoleLog . ms
  dispose ambientLight1
  ambientLight2 <- THREE.AmbientLight.new 0 0.42
  copy ambientLight1 ambientLight2
  intensity ambientLight1 >>= consoleLog . ms

  pointLight1 <- THREE.AmbientLight.new 0 0.03
  valToStr (unAmbientLight pointLight1) >>= consoleLog
  intensity pointLight1 >>= consoleLog . ms

app :: JSM ()
app = do
  consoleLog "begin"
  testLights
  consoleLog "end"

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run app

