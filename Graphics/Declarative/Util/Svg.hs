module Graphics.Declarative.Util.Svg where

import Control.Monad
import System.IO
import Graphics.Rendering.Cairo.SVG
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Graphic
import Graphics.Declarative.Frame
import Graphics.Declarative.Framed
import Graphics.Declarative.Backend.Cairo

-- Origin on top-left
svgForm :: SVG -> Form
svgForm svg
  = Framed (Frame 0 0 (fromIntegral w) (fromIntegral h)) $ primitive $ do
      success <- svgRender svg
      unless success $ liftIO $
        putStrLn "Warning: Couldn't render SVG. (Graphics.Declarative.Util.SVG)"
  where (w, h) = svgGetSize svg

-- Origin on top-left
svgFormString :: String -> IO Form
svgFormString str = svgForm `fmap` svgNewFromString str

-- Origin on top-left
svgFormHandle :: Handle -> IO Form
svgFormHandle handle = svgForm `fmap` svgNewFromHandle handle

-- Origin on top-left
svgFormFile :: FilePath -> IO Form
svgFormFile file = svgForm `fmap` svgNewFromFile file
