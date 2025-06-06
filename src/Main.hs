{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle as JS
import Miso 
import Miso.Media
import Miso.String 

import THREE.Mesh

-------------------------------------------------------------------------------
-- test app
-------------------------------------------------------------------------------

app :: JSM ()
app = do
  consoleLog "begin"

  Image image1 <- newImage "favicon.ico"
  valToStr image1 >>= consoleLog

  -----------------------------------------------------------------------------
  -- doesn't work since Miso.Media.new try to create a 'Media' object (but JS
  -- only knows Audio or Video).

  -- Media media1 <- Miso.Media.new "favicon.ico"
  -- valToStr media1 >>= consoleLog
  -----------------------------------------------------------------------------

  mesh1 <- THREE.Mesh.new
  valToStr (unMesh mesh1) >>= consoleLog

  consoleLog "end"

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run app

