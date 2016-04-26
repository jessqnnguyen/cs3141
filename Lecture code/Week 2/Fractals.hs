import Graphics.SpriteKit hiding (Path)

-- draws spiral rays
spiralRays :: Int -> Colour -> Line -> Picture
spiralRays 0 _colour _line = []
-- recursively call spiralRays
spiralRays n colour line 
 		= (colour), [p1, p2]) : spiralRays (n-1) (fade colour) newLine
	where
		(p1,p2) = line
		-- scaleLine takes a scaling factor
		newLine = scaleLine 0.9963  (rotateLine (pi/123) line)

let line = ((400, 400), (620, 620))

drawPicture 1 (spiralRays (pi/123) 0.9963 100 red line)

fractalTree :: Float -> Int -> Line -> Path
fractalTree factor 0 line = 
