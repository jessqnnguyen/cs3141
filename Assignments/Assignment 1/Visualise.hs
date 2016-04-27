module Visualise (visualiseWorld) where

import World
import Graphics.SpriteKit

-- Visualise the world
--
visualiseWorld :: World -> Node u
visualiseWorld (World sf unitMass _ ps) = (node (map visualiseParticle ps))
                                          { nodeXScale = (floatToDouble sf)
                                          , nodeYScale = (floatToDouble sf)
                                          }
  where
    visualiseParticle (Particle m (x, y) _) = (solidCircle size)
                                              { nodePosition = Point (floatToDouble x) 
                                                                     (floatToDouble y)
                                              }
      where
        size = logBase 10 (m / unitMass) / sf

solidCircle :: Float -> Node u
solidCircle r = (shapeNodeWithPath circlePath)
                { shapeFillColor = orangeColor
                , shapeLineWidth = 0
                }
  where
    rd         = floatToDouble r
    circlePath = [ MoveToPoint (Point 0 rd)
                 , AddCurveToPoint (Point k rd) (Point rd k) (Point rd 0)
                 , AddCurveToPoint (Point rd (-k)) (Point k (-rd)) (Point 0 (-rd))
                 , AddCurveToPoint (Point (-k) (-rd)) (Point (-rd) (-k)) (Point (-rd) 0)
                 , AddCurveToPoint (Point (-rd) k) (Point (-k) rd) (Point 0 rd)
                 ] 
    k          = 0.5522847498 * rd

floatToDouble :: Float -> Double
floatToDouble = fromRational . toRational