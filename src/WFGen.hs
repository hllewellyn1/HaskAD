module WFGen where

import Graphics.Gloss
import Geom

-------------------------------------------------------------------

-- interpolate solves some line equations to generate a wireframe model

-- incrConst is proportional to the number of points between each vertex (1 is a good number)

incrConst :: Float
incrConst = 1

interpolate :: [Coord] -> [Coord]
interpolate [] = []
interpolate [x] = [x]
interpolate [Vertex (a,b,c),Vertex (d,e,f)]
	| (a<d)&&(b<e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a+incrConst)..d], y <- [b,(b+incrConst)..e], z <- [c,(c+incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (a<d)&&(e<b)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a+incrConst)..d], y <- [b,(b-incrConst)..e], z <- [c,(c+incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (a<d)&&(b<e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a+incrConst)..d], y <- [b,(b+incrConst)..e], z <- [c,(c-incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (a<d)&&(e<b)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a+incrConst)..d], y <- [b,(b-incrConst)..e], z <- [c,(c-incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (a<d)&&(b==e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,b,z) | x <- [a,(a+(incrConst/10))..d], z <- [c,(c+(incrConst/10))..f], (x-a)/(d-a)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a<d)&&(b<e)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,y,c) | x <- [a,(a+(incrConst/10))..d], y <- [b,(b+(incrConst/10))..e], (x-a)/(d-a)==(y-b)/(e-b)] ++ [Vertex (d,e,f)]
	| (a<d)&&(b==e)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,b,c) | x <- [a,(a+(incrConst/10))..d]] ++ [Vertex (d,e,f)]
	| (a<d)&&(b==e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (x,b,z) | x <- [a,(a+(incrConst/10))..d], z <- [c,(c-(incrConst/10))..f], (x-a)/(d-a)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a<d)&&(e<b)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,y,c) | x <- [a,(a+(incrConst/10))..d], y <- [b,(b-(incrConst/10))..e], (x-a)/(d-a)==(y-b)/(e-b)] ++ [Vertex (d,e,f)]
	{--}
	| (d<a)&&(b<e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a-incrConst)..d], y <- [b,(b+incrConst)..e], z <- [c,(c+incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (d<a)&&(e<b)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a-incrConst)..d], y <- [b,(b-incrConst)..e], z <- [c,(c+incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (d<a)&&(b<e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a-incrConst)..d], y <- [b,(b+incrConst)..e], z <- [c,(c-incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (d<a)&&(e<b)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (x,y,z) | x <- [a,(a-incrConst)..d], y <- [b,(b-incrConst)..e], z <- [c,(c-incrConst)..f], ((x-a)/(d-a)==(y-b)/(e-b))&&((y-b)/(e-b)==(z-c)/(f-c))] ++ [Vertex (d,e,f)]
	| (d<a)&&(b==e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (x,b,z) | x <- [a,(a-(incrConst/10))..d], z <- [c,(c+(incrConst/10))..f], (x-a)/(d-a)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (d<a)&&(b<e)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,y,c) | x <- [a,(a-(incrConst/10))..d], y <- [b,(b+(incrConst/10))..e], (x-a)/(d-a)==(y-b)/(e-b)] ++ [Vertex (d,e,f)]
	| (d<a)&&(b==e)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,b,c) | x <- [a,(a-(incrConst/10))..d]] ++ [Vertex (d,e,f)]
	| (d<a)&&(b==e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (x,b,z) | x <- [a,(a-(incrConst/10))..d], z <- [c,(c-(incrConst/10))..f], (x-a)/(d-a)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (d<a)&&(e<b)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (x,y,c) | x <- [a,(a-(incrConst/10))..d], y <- [b,(b-(incrConst/10))..e], (x-a)/(d-a)==(y-b)/(e-b)] ++ [Vertex (d,e,f)]
	{--}
	| (a==d)&&(b<e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (a,y,z) | y <- [b,(b+(incrConst/10))..e], z <- [c,(c+(incrConst/10))..f], (y-b)/(e-b)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a==d)&&(e<b)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (a,y,z) | y <- [b,(b-(incrConst/10))..e], z <- [c,(c+(incrConst/10))..f], (y-b)/(e-b)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a==d)&&(b<e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (a,y,z) | y <- [b,(b+(incrConst/10))..e], z <- [c,(c-(incrConst/10))..f], (y-b)/(e-b)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a==d)&&(e<b)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (a,y,z) | y <- [b,(b-(incrConst/10))..e], z <- [c,(c-(incrConst/10))..f], (y-b)/(e-b)==(z-c)/(f-c)] ++ [Vertex (d,e,f)]
	| (a==d)&&(b==e)&&(c<f)	= [Vertex (a,b,c)] ++ [Edge (a,b,z) | z <- [c,(c+(incrConst/10))..f]] ++ [Vertex (d,e,f)]
	| (a==d)&&(b<e)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (a,y,c) | y <- [b,(b+(incrConst/10))..e]] ++ [Vertex (d,e,f)]
	| (a==d)&&(b==e)&&(f<c)	= [Vertex (a,b,c)] ++ [Edge (a,b,z) | z <- [c,(c-(incrConst/10))..f]] ++ [Vertex (d,e,f)]
	| (a==d)&&(e<b)&&(c==f)	= [Vertex (a,b,c)] ++ [Edge (a,y,c) | y <- [b,(b-(incrConst/10))..e]] ++ [Vertex (d,e,f)]
interpolate (x:xs) = (x:xs)

-------------------------------------------------------------------

-- some helper functions to process / clean up the output data

splitListIntoTuples :: [Coord] -> [((Coord,Coord))]
splitListIntoTuples xs = zip xs (tail xs)

splitTuplesIntoLists :: ((a),(a)) -> [a]
splitTuplesIntoLists ((x),(y)) = [x,y]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   
	| x `elem` xs   = rmdups xs
    | otherwise     = x : rmdups xs
	
minus50 :: Coord -> Coord
minus50 (Vertex (x,y,z)) = Vertex (x-50,y-50,z-50) 
minus50 (Edge (x,y,z)) = Edge (x-50,y-50,z-50) 
	
-------------------------------------------------------------------

-- genWireFrame is a composition of the above functions

genWireFrame :: [Coord] -> [Coord]
genWireFrame x =  (rmdups (concat (map interpolate (map splitTuplesIntoLists (splitListIntoTuples x)))))



