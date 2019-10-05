module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Console (log)

import Web.DOM.Document (Document, createElement) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (appendChild) as DOM

import Web.HTML (window) as HTML
import Web.HTML.Window as HTML.Window
import Web.HTML.HTMLDocument (body, toDocument) as HTML
import Web.HTML.HTMLElement as HTML.HTMLElement

import Direction (Direction(..))

foreign import setStyleProp :: String -> String -> DOM.Element.Element -> Effect Boolean

type State = { dir :: Direction
    , position :: Number
    , rafId :: HTML.Window.RequestAnimationFrameId
}

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

getNewDirectionAndDist :: Direction -> Number -> Int -> Effect (Tuple Direction Number)
getNewDirectionAndDist dir distValPx width = case dir of 
                            RightDir -> if distValPx >= (toNumber width) - 100.0
                                then pure (Tuple LeftDir distValPx)
                                else pure $ Tuple RightDir (distValPx + 9.0)
                            _ -> if distValPx <= 0.0
                                then pure (Tuple RightDir distValPx)
                                else pure $ Tuple LeftDir (distValPx - 9.0)

moveBox :: Direction -> DOM.Element.Element -> Ref State -> Effect Unit
moveBox hDir el stateRef = do
    -- Read state
    state <- read stateRef
    
    -- Move box
    let distValPx = state.position
        distStr   = (show distValPx) <> "px"

    _ <- setStyleProp "transform" ( "translate(" <> distStr <> ", 0)" ) el

    w <- HTML.window
    width <- HTML.Window.innerWidth w

    Tuple direction newDist <- getNewDirectionAndDist hDir distValPx width

    -- Call next frame
    animationFrameId <- HTML.Window.requestAnimationFrame (moveBox direction el stateRef) w

    -- Update state
    write { dir: direction
        , position: newDist
        , rafId: animationFrameId
    } stateRef

execFrame :: Direction -> DOM.Element.Element -> Ref State -> Effect Unit
execFrame hDir el stateRef = moveBox hDir el stateRef

main :: Effect Unit
main = do
  -- Get window and document objects
  w <- HTML.window
  d <- HTML.Window.document w
  mBody <- HTML.body d
  defaultElem <- (DOM.createElement "span" (HTML.toDocument d))

  -- Create frame that does nothing just to get default frame id
  defaultId <- (HTML.Window.requestAnimationFrame (pure unit) w)

  -- Default state
  stateRef <- new {
    dir: RightDir,
    position: 0.0,
    rafId: defaultId
  }

  let b = case mBody of 
        Nothing -> DOM.Element.toNode (defaultElem)
        Just b'  -> HTML.HTMLElement.toNode b'

  boxEl <- createBoxElement "the-box" $ HTML.toDocument d
  newBody <- DOM.appendChild (DOM.Element.toNode boxEl) b

  frameId <- HTML.Window.requestAnimationFrame (
    execFrame RightDir boxEl stateRef
  ) w
  pure unit
