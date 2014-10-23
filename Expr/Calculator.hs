import Control.Monad (when)

import Haste hiding (eval)
import Haste.Graphics.Canvas

import Expr
import Pages
import Data.Maybe

canWidth  = 300
canHeight = 300

readAndDraw :: Elem -> Canvas -> IO ()
readAndDraw input can = do
                         s <- getProp input "value"
                         case readExpr s of
                            Just a  -> render can $ stroke $ path $ points a 0.04 (canWidth, canHeight)
                            Nothing -> alert "Error"
                         

main = do
    -- Elements
    canvas  <- mkCanvas canWidth canHeight   -- The drawing area
    fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
    input   <- mkInput 20 "x"                -- The formula input
    draw    <- mkButton "Draw graph"         -- The draw button
      -- The markup "<i>...</i>" means that the text inside should be rendered
      -- in italics.

    -- Layout
    formula <- mkDiv
    row formula [fx,input]
    column documentBody [canvas,formula,draw]

    -- Styling
    setStyle documentBody "backgroundColor" "lightblue"
    setStyle documentBody "textAlign" "center"
    setStyle input "fontSize" "14pt"
    focus input
    select input

    -- Interaction
    Just can <- getCanvas canvas
    onEvent draw  OnClick $ \_ _  -> readAndDraw input can
    onEvent input OnKeyUp $ \code -> when (code==13) $ readAndDraw input can
      -- "Enter" key has code 13

-----------------------------------------------------------------------------

points :: Expr -> Double -> (Int,Int) -> [Point]
points expr scale (width, height) = [ (a, realToPix (eval expr (pixToReal a))) | a <- [0..(fromIntegral width)] ]
   where
    pixToReal :: Double -> Double
    pixToReal x = (x - (fromIntegral width) / 2) * scale

    realToPix :: Double -> Double
    realToPix y = (-y / scale) + (fromIntegral height) / 2       