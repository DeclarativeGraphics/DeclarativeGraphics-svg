module Graphics.Declarative.Util.Svg where

import Control.Monad
import System.IO
import Graphics.Rendering.Cairo.SVG
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Graphic
import Graphics.Declarative.Envelope
import Graphics.Declarative.Enveloped
import Graphics.Declarative.Backend.Cairo

-- Origin on top-left
svgForm :: SVG -> CairoEGraphic
svgForm svg =
  let (w, h) = svgGetSize svg
   in Pack (Envelope 0 0 (fromIntegral w) (fromIntegral h)) $ Leaf $ do
    success <- svgRender svg
    unless success $ liftIO $ putStrLn "Warning: Couldn't render SVG. (Graphics.Declarative.Util.SVG)"

-- Origin on top-left
svgFormString :: String -> IO CairoEGraphic
svgFormString str = svgForm `fmap` svgNewFromString str

-- Origin on top-left
svgFormHandle :: Handle -> IO CairoEGraphic
svgFormHandle handle = svgForm `fmap` svgNewFromHandle handle

-- Origin on top-left
svgFormFile :: FilePath -> IO CairoEGraphic
svgFormFile file = svgForm `fmap` svgNewFromFile file
