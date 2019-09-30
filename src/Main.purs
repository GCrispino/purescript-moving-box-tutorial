module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

import Web.DOM.Document (Document, createElement) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (appendChild) as DOM

import Web.HTML (window) as HTML
import Web.HTML.Window as HTML.Window
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLElement as HTML.HTMLElement

foreign import setStyleProp :: String -> String -> DOM.Element.Element -> Effect Boolean

createBoxElement :: String -> DOM.Document -> Effect DOM.Element.Element
createBoxElement id document = do
    boxEl <- DOM.createElement "div" document
    DOM.Element.setId id boxEl
    DOM.Element.setClassName "box" boxEl
    _ <- setStyleProp "position" "relative" boxEl
    _ <- setStyleProp "width" "5em" boxEl
    _ <- setStyleProp "height" "5em" boxEl
    _ <- setStyleProp "background" "#ff4242" boxEl
    pure boxEl

execFrame :: Effect Unit
execFrame = do
        log "Request animation frame!!"
        w <- HTML.window
        -- Call next frame
        animationFrameId <- HTML.Window.requestAnimationFrame (execFrame) w
        pure unit


main :: Effect Unit
main = do
  -- Get window and document objects
  w <- HTML.window
  d <- HTML.Window.document w
  mBody <- HTML.body d
  defaultElem <- (DOM.createElement "span" (HTML.toDocument d))

  let b = case mBody of 
        Nothing -> DOM.Element.toNode (defaultElem)
        Just b'  -> HTML.HTMLElement.toNode b'

  boxEl <- createBoxElement "the-box" $ HTML.toDocument d
  newBody <- DOM.appendChild (DOM.Element.toNode boxEl) b

  frameId <- HTML.Window.requestAnimationFrame (
    execFrame
  ) w
  pure unit
