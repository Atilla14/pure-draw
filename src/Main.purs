module Main where

import Prelude
import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas as C
import Signal
import Signal.Time
import Signal.DOM (animationFrame, keyPressed, mouseButton, mousePos)
import Data.Tuple
import Partial.Unsafe
import Data.Int (toNumber)

spaceBarKey :: Int
spaceBarKey = 32

main = unsafePartial $ do
  Just canvas <- C.getCanvasElementById "canvas"
  ctx <- C.getContext2D canvas
  frames <- animationFrame
  mouseClicks <- mouseButton 0
  spaceBar <- keyPressed spaceBarKey
  mouseCoordinates <- mousePos
  let inputs = {pos: _, click: _, spaceBar: _} <~ mouseCoordinates ~ mouseClicks ~ spaceBar
  let game = foldp update initialState (sampleOn frames inputs)
  runSignal (render ctx <$> game)

initialState = {
                 current : {x: 0, y: 0}
               , previous: {x: 0, y: 0}
               , random: false
               }

update inputs state =
  if inputs.click && (not inputs.spaceBar)
  then { current: inputs.pos, previous: state.current, random: false}
  else if inputs.click && inputs.spaceBar
  then { current: inputs.pos, previous: state.current, random: true}
  else {current: inputs.pos, previous: inputs.pos, random: false}

render ctx state = do
  _ <- draw ctx state
  pure unit

setStrokeStyle ctx state
  | state.random == true = C.setStrokeStyle "blue" ctx
  | otherwise            = C.setStrokeStyle "red" ctx

draw ctx state = do
  _ <- setStrokeStyle ctx state
  _ <- C.beginPath ctx
  _ <- C.moveTo ctx prevX prevY
  _ <- C.lineTo ctx currX currY
  _ <- C.stroke ctx
  pure ctx
    where
      prevX = toNumber state.previous.x
      prevY = toNumber state.previous.y
      currX = toNumber state.current.x
      currY = toNumber state.current.y

