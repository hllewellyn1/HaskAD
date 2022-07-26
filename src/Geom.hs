module Geom where

import Graphics.Gloss

-----------------------------------------

-- data definitions

data Coord = Vertex (Float,Float,Float) | Edge (Float,Float,Float) | AxisPointX (Float,Float,Float) | AxisPointY (Float,Float,Float) | AxisPointZ (Float,Float,Float)
	deriving (Eq, Show, Read)
	
data Coord2 = Vertex2 (Float, Float) | Edge2 (Float, Float) | AxisPointX2 (Float, Float) | AxisPointY2 (Float, Float) | AxisPointZ2 (Float, Float)

-- we use a 'type' def here for sake of readability

type Obj = [Coord]

-- some special coordinates and some special objects

xyOrigin :: Coord2
xyOrigin = Vertex2 (0,0)

xAxis :: Obj
xAxis = [AxisPointX (x,0,0) | x <- [1,2..150]]

yAxis :: Obj
yAxis = [AxisPointY (0,y,0) | y <- [1,2..150]]

zAxis :: Obj
zAxis = [AxisPointZ (0,0,z) | z <- [1,2..150]]

axes :: Obj
axes = xAxis ++ yAxis ++ zAxis

-------------------------------------------------------------------

-- 3D transformations

identity :: Coord -> Coord
identity (Vertex (x,y,z)) = Vertex (((1*x)+(0*y)+(0*z)),((0*x)+(1*y)+(0*z)),((0*x)+(0*y)+(1*z)))
identity (Edge (x,y,z)) = Edge (((1*x)+(0*y)+(0*z)),((0*x)+(1*y)+(0*z)),((0*x)+(0*y)+(1*z)))
identity (AxisPointX (x,y,z)) = AxisPointX (((1*x)+(0*y)+(0*z)),((0*x)+(1*y)+(0*z)),((0*x)+(0*y)+(1*z)))
identity (AxisPointY (x,y,z)) = AxisPointY (((1*x)+(0*y)+(0*z)),((0*x)+(1*y)+(0*z)),((0*x)+(0*y)+(1*z)))
identity (AxisPointZ (x,y,z)) = AxisPointZ (((1*x)+(0*y)+(0*z)),((0*x)+(1*y)+(0*z)),((0*x)+(0*y)+(1*z)))

-- these rotation matrices are mostly redundant (they don't account for the axes being rotated).  they
-- are used in the initial rotation of the axes, but otherwise we use 'rotation' (see below)

rotationX :: Float -> Coord -> Coord
rotationX n (Vertex (x,y,z)) = Vertex (x, (cos n)*y-(sin n)*z, (sin n)*y+(cos n)*z)
rotationX n (Edge (x,y,z)) = Edge (x, (cos n)*y-(sin n)*z, (sin n)*y+(cos n)*z)
rotationX n (AxisPointX (x,y,z)) = AxisPointX (x, (cos n)*y-(sin n)*z, (sin n)*y+(cos n)*z)
rotationX n (AxisPointY (x,y,z)) = AxisPointY (x, (cos n)*y-(sin n)*z, (sin n)*y+(cos n)*z)
rotationX n (AxisPointZ (x,y,z)) = AxisPointZ (x, (cos n)*y-(sin n)*z, (sin n)*y+(cos n)*z)

rotationY :: Float -> Coord -> Coord
rotationY n (Vertex (x,y,z)) = Vertex ((cos n)*x+(sin n)*z, y, (-1)*(sin n)*x + (cos n)*z)
rotationY n (Edge (x,y,z)) = Edge ((cos n)*x+(sin n)*z, y, (-1)*(sin n)*x + (cos n)*z)
rotationY n (AxisPointX (x,y,z)) = AxisPointX ((cos n)*x+(sin n)*z, y, (-1)*(sin n)*x + (cos n)*z)
rotationY n (AxisPointY (x,y,z)) = AxisPointY ((cos n)*x+(sin n)*z, y, (-1)*(sin n)*x + (cos n)*z)
rotationY n (AxisPointZ (x,y,z)) = AxisPointZ ((cos n)*x+(sin n)*z, y, (-1)*(sin n)*x + (cos n)*z)

rotationZ :: Float -> Coord -> Coord
rotationZ n (Vertex (x,y,z)) = Vertex ((((cos n)*x) - ((sin n)*y)),(((sin n)*x)+((cos n)*y)),(z))
rotationZ n (Edge (x,y,z)) = Edge ((((cos n)*x) - ((sin n)*y)),(((sin n)*x)+((cos n)*y)),(z))
rotationZ n (AxisPointX (x,y,z)) = AxisPointX ((((cos n)*x) - ((sin n)*y)),(((sin n)*x)+((cos n)*y)),(z))
rotationZ n (AxisPointY (x,y,z)) = AxisPointY ((((cos n)*x) - ((sin n)*y)),(((sin n)*x)+((cos n)*y)),(z))
rotationZ n (AxisPointZ (x,y,z)) = AxisPointZ ((((cos n)*x) - ((sin n)*y)),(((sin n)*x)+((cos n)*y)),(z))

-- coord here is the coordinate of the endpoint of the unit axis vector around which we rotate

rotation :: Coord -> Float -> Coord -> Coord
rotation (AxisPointX (u,v,w)) n (Vertex (x,y,z)) = Vertex (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointX (u,v,w)) n (Edge (x,y,z)) = Edge (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointX (u,v,w)) n (AxisPointX (x,y,z)) = AxisPointX (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointX (u,v,w)) n (AxisPointY (x,y,z)) = AxisPointY (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointX (u,v,w)) n (AxisPointZ (x,y,z)) = AxisPointZ (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
{--}
rotation (AxisPointY (u,v,w)) n (Vertex (x,y,z)) = Vertex (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,((u*w*(1-(cos n))-v*(sin n)))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointY (u,v,w)) n (Edge (x,y,z)) = Edge (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointY (u,v,w)) n (AxisPointX (x,y,z)) = AxisPointX (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointY (u,v,w)) n (AxisPointY (x,y,z)) = AxisPointY (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointY (u,v,w)) n (AxisPointZ (x,y,z)) = AxisPointZ (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
{--}
rotation (AxisPointZ (u,v,w)) n (Vertex (x,y,z)) = Vertex (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointZ (u,v,w)) n (Edge (x,y,z)) = Edge (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointZ (u,v,w)) n (AxisPointX (x,y,z)) = AxisPointX (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointZ (u,v,w)) n (AxisPointY (x,y,z)) = AxisPointY (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))
rotation (AxisPointZ (u,v,w)) n (AxisPointZ (x,y,z)) = AxisPointZ (((u*u*(1-(cos n))+(cos n))*x + (u*v*(1-(cos n))-w*(sin n))*y + (u*w*(1-(cos n))+v*(sin n))*z,(u*v*(1-(cos n))+w*(sin n))*x + (v*v*(1-(cos n))+(cos n))*y + (v*w*(1-(cos n))-u*(sin n))*z,(u*w*(1-(cos n))-v*(sin n))*x + (v*w*(1-(cos n))+u*(sin n))*y + (w*w*(1-(cos n))+(cos n))*z))

-- isometric projection matrix

project :: Coord -> Coord2
project (Vertex (x,y,z)) = Vertex2 (((sqrt 3)/(sqrt 6)*x)-((sqrt 3)/(sqrt 6))*z,((x/(sqrt 6))+(2*y)/(sqrt 6)+(z/(sqrt 6))))
project (Edge (x,y,z)) = Edge2 (((sqrt 3)/(sqrt 6)*x)-((sqrt 3)/(sqrt 6))*z,((x/(sqrt 6))+(2*y)/(sqrt 6)+(z/(sqrt 6))))
project (AxisPointX (x,y,z)) = AxisPointX2 (((sqrt 3)/(sqrt 6)*x)-((sqrt 3)/(sqrt 6))*z,((x/(sqrt 6))+(2*y)/(sqrt 6)+(z/(sqrt 6))))
project (AxisPointY (x,y,z)) = AxisPointY2 (((sqrt 3)/(sqrt 6)*x)-((sqrt 3)/(sqrt 6))*z,((x/(sqrt 6))+(2*y)/(sqrt 6)+(z/(sqrt 6))))
project (AxisPointZ (x,y,z)) = AxisPointZ2 (((sqrt 3)/(sqrt 6)*x)-((sqrt 3)/(sqrt 6))*z,((x/(sqrt 6))+(2*y)/(sqrt 6)+(z/(sqrt 6))))

-----------------------------------------

-- drawing functions

drawCoord2 :: Coord2 -> Picture
drawCoord2 (Vertex2 (x,y)) = translate x y (Color yellow (circleSolid 3.0))
drawCoord2 (Edge2 (x,y)) = translate x y (Color white (circleSolid 1.0))
drawCoord2 (AxisPointX2 (x,y)) = translate x y (Color red (circleSolid 0.8))
drawCoord2 (AxisPointY2 (x,y)) = translate x y (Color green (circleSolid 0.8))
drawCoord2 (AxisPointZ2 (x,y)) = translate x y (Color blue (circleSolid 0.8))

-----------------------------------------

-- misc. geometric functions

normalise :: Coord -> Coord
normalise (Vertex (x,y,z)) = (Vertex (x/(sqrt (x*x+y*y+z*z)),y/(sqrt (x*x+y*y+z*z)),z/(sqrt (x*x+y*y+z*z))))
normalise (Edge (x,y,z)) = (Edge (x/(sqrt (x*x+y*y+z*z)),y/(sqrt (x*x+y*y+z*z)),z/(sqrt (x*x+y*y+z*z))))
normalise (AxisPointX (x,y,z)) = (AxisPointX (x/(sqrt (x*x+y*y+z*z)),y/(sqrt (x*x+y*y+z*z)),z/(sqrt (x*x+y*y+z*z))))
normalise (AxisPointY (x,y,z)) = (AxisPointY (x/(sqrt (x*x+y*y+z*z)),y/(sqrt (x*x+y*y+z*z)),z/(sqrt (x*x+y*y+z*z))))
normalise (AxisPointZ (x,y,z)) = (AxisPointZ (x/(sqrt (x*x+y*y+z*z)),y/(sqrt (x*x+y*y+z*z)),z/(sqrt (x*x+y*y+z*z))))


