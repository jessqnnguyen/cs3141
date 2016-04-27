module Main where

import Graphics.SpriteKit

import World
import Visualise
import Physics
import Simulation


data WorldState = Initial World
                | Timed   World TimeInterval 

-- Launch the simulation
--
simulate :: World -> Scene WorldState ()
simulate world = (sceneWithSize (Size (fromIntegral width) (fromIntegral height)))
                 { sceneChildren    = [visualiseWorld world]
                 , sceneAnchorPoint = Point 0.5 0.5
                 , sceneData        = Initial world
                 , sceneUpdate      = Just nextScene
                 }
  where
    nextScene scene@Scene{ sceneData = Initial world } time
      = scene{ sceneData = Timed world time }
    nextScene scene@Scene{ sceneData = Timed world time } newTime
      = scene
        { sceneData     = Timed newWorld newTime
        , sceneChildren = [visualiseWorld newWorld]
        }
      where
        dt       = newTime - time
        newWorld = advanceWorld undefined (fromRational (toRational dt)) world
