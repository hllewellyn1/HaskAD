module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..), SpecialKey (..) )
import qualified Graphics.Gloss.Interface.IO.Interact as K (KeyState (..))
import System.IO
import System.Exit
import Data.List

import Geom
import Parser
import WFGen


----------------------------------------------------------------------------

-- world datatype
-- constructor, state of program, model to be displayed, x-axis vector, y-axis vecotr, z-axis vector, text-cache, vertex-cache 1, vertex-cache 2 

data World = World ProgramState Obj Coord Coord Coord [Char] (Coord) [Coord]
	deriving (Eq,Show)
	
data ProgramState = Title | EditX | EditY | EditZ | View
	deriving (Eq, Show)
	
-- we need to perform an initial rotation because the standard isometric projection matrix
-- leaves the axes in a weird configuration (e.g. z axis not vertical)
	
initWorld :: Obj -> World
initWorld x = World View (map (\a -> rotationY (pi/2) a) (map (\a -> rotationX (-pi/2) a) (x ++ axes))) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointX (1,0,0))))) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointY (0,1,0))))) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointZ (0,0,1))))) [] (Vertex (0,0,0)) []

----------------------------------------------------------------------------

-- functions to be passed to play

windowInfo :: Display
windowInfo = InWindow "HasKAD" (512, 512) (10, 10)

bgColor :: Color
bgColor = black

grey :: Color
grey = mixColors 0.5 0.5 white black

----------------------------------------------------------------------------

-- render function

renderFunction :: World -> IO Picture
renderFunction = \(World x y u v w a (Vertex (b1,b2,b3)) c) -> case x of
	Title -> pure (color white (Text "Title"))
	EditX -> pure (pictures ((map drawCoord2 (map project y)) ++ [color white (rectangleSolid 300 200)] ++ [scale 0.2 0.2 (color black (translate (-680) 320 (text "Add New Point:")))] ++ [scale 0.15 0.15 (color black (translate (-800) 200 (text "x :")))] ++ [scale 0.15 0.15 (color black (translate (-800) 0 (text "y :")))] ++ [scale 0.15 0.15 (color black (translate (-800) (-200) (text "z :")))] ++ [scale 0.14 0.14 (color black (translate (-950) (-560) (text "Press Enter to Confirm Point")))] ++ [translate (-20) 35 (color grey (rectangleWire 100 20))] ++ [translate (-20) (5) (color grey (rectangleSolid 100 20))] ++ [translate (-20) (-25) (color grey (rectangleSolid 100 20))] ++ [scale 0.12 0.12 (color black (translate (-550) 250 (text (show b1))))] ++ [scale 0.12 0.12 (color black (translate (-550) 0 (text (show b2))))] ++ [scale 0.12 0.12 (color black (translate (-550) (-250) (text (show b3))))] ++ [scale 0.2 0.2 (color red (translate (-1250) (1180) (text "x")))] ++ [scale 0.2 0.2 (color green (translate (-1150) (1180) (text "y")))] ++ [scale 0.2 0.2 (color blue (translate (-1050) (1180) (text "z")))])                                  )
	EditY -> pure (pictures ((map drawCoord2 (map project y)) ++ [color white (rectangleSolid 300 200)] ++ [scale 0.2 0.2 (color black (translate (-680) 320 (text "Add New Point:")))] ++ [scale 0.15 0.15 (color black (translate (-800) 200 (text "x :")))] ++ [scale 0.15 0.15 (color black (translate (-800) 0 (text "y :")))] ++ [scale 0.15 0.15 (color black (translate (-800) (-200) (text "z :")))] ++ [scale 0.14 0.14 (color black (translate (-950) (-560) (text "Press Enter to Confirm Point")))] ++ [translate (-20) 35 (color grey (rectangleSolid 100 20))] ++ [translate (-20) (5) (color grey (rectangleWire 100 20))] ++ [translate (-20) (-25) (color grey (rectangleSolid 100 20))] ++ [scale 0.12 0.12 (color black (translate (-550) 250 (text (show b1))))] ++ [scale 0.12 0.12 (color black (translate (-550) 0 (text (show b2))))] ++ [scale 0.12 0.12 (color black (translate (-550) (-250) (text (show b3))))] ++ [scale 0.2 0.2 (color red (translate (-1250) (1180) (text "x")))] ++ [scale 0.2 0.2 (color green (translate (-1150) (1180) (text "y")))] ++ [scale 0.2 0.2 (color blue (translate (-1050) (1180) (text "z")))]))
	EditZ -> pure (pictures ((map drawCoord2 (map project y)) ++ [color white (rectangleSolid 300 200)] ++ [scale 0.2 0.2 (color black (translate (-680) 320 (text "Add New Point:")))] ++ [scale 0.15 0.15 (color black (translate (-800) 200 (text "x :")))] ++ [scale 0.15 0.15 (color black (translate (-800) 0 (text "y :")))] ++ [scale 0.15 0.15 (color black (translate (-800) (-200) (text "z :")))] ++ [scale 0.14 0.14 (color black (translate (-950) (-560) (text "Press Enter to Confirm Point")))] ++ [translate (-20) 35 (color grey (rectangleSolid 100 20))] ++ [translate (-20) (5) (color grey (rectangleSolid 100 20))] ++ [translate (-20) (-25) (color grey (rectangleWire 100 20))] ++ [scale 0.12 0.12 (color black (translate (-550) 250 (text (show b1))))] ++ [scale 0.12 0.12 (color black (translate (-550) 0 (text (show b2))))] ++ [scale 0.12 0.12 (color black (translate (-550) (-250) (text (show b3))))] ++ [scale 0.2 0.2 (color red (translate (-1250) (1180) (text "x")))] ++ [scale 0.2 0.2 (color green (translate (-1150) (1180) (text "y")))] ++ [scale 0.2 0.2 (color blue (translate (-1050) (1180) (text "z")))]))
	View -> pure (pictures ((map drawCoord2 (map project y)) ++ [scale 0.2 0.2 (color red (translate (-1250) (1180) (text "x")))] ++ [scale 0.2 0.2 (color green (translate (-1150) (1180) (text "y")))] ++ [scale 0.2 0.2 (color blue (translate (-1050) (1180) (text "z")))]))

----------------------------------------------------------------------------

-- input handler function

inputHandlerFunction :: Event -> World -> IO World
-- rotation about x axis
inputHandlerFunction (EventKey (Char 'x') K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (map (\a -> rotation u (pi/12) a) y) u (normalise (rotation u (pi/12) v)) (normalise (rotation u (pi/12) w)) a (Vertex (b1,b2,b3)) c))
-- rotation about y axis
inputHandlerFunction (EventKey (Char 'y') K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (map (\a -> rotation v (pi/12) a) y) (normalise (rotation v (pi/12) u)) v (normalise (rotation v (pi/12) w)) a (Vertex (b1,b2,b3)) c))
-- rotation about z axis
inputHandlerFunction (EventKey (Char 'z') K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (map (\a -> rotation w (pi/12) a) y) (normalise (rotation w (pi/12) u)) (normalise (rotation w (pi/12) v)) w a (Vertex (b1,b2,b3)) c))
-- enter edit mode
inputHandlerFunction (EventKey (SpecialKey KeySpace) K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w a (Vertex (b1,b2,b3)) c))
-- exit edit mode
inputHandlerFunction (EventKey (SpecialKey KeySpace) K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (y++(genWireFrame c)) u v w [] (Vertex (b1,b2,b3)) []))
inputHandlerFunction (EventKey (SpecialKey KeySpace) K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (y++(genWireFrame c)) u v w [] (Vertex (b1,b2,b3)) []))
inputHandlerFunction (EventKey (SpecialKey KeySpace) K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World View (y++(genWireFrame c)) u v w [] (Vertex (b1,b2,b3)) []))
-- change to edit x mode
inputHandlerFunction (EventKey (Char 'x') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w [] (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char 'x') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w [] (Vertex (b1,b2,b3)) c))
-- change to edit y mode
inputHandlerFunction (EventKey (Char 'y') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w [] (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char 'y') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w [] (Vertex (b1,b2,b3)) c))
-- change to edit z mode
inputHandlerFunction (EventKey (Char 'z') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w [] (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char 'z') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w [] (Vertex (b1,b2,b3)) c))
-- confirm point
inputHandlerFunction (EventKey (SpecialKey KeyEnter) K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w [] (Vertex (0,0,0)) (c++[Vertex (b1,b2,b3)])))
inputHandlerFunction (EventKey (SpecialKey KeyEnter) K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w [] (Vertex (0,0,0)) (c++[Vertex (b1,b2,b3)])))
inputHandlerFunction (EventKey (SpecialKey KeyEnter) K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w [] (Vertex (0,0,0)) (c++[Vertex (b1,b2,b3)])))
-- text entry in edit x mode
inputHandlerFunction (EventKey (Char '0') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['0']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '1') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['1']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '2') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['2']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '3') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['3']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '4') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['4']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '5') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['5']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '6') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['6']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '7') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['7']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '8') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['8']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '9') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditX y u v w (a++['9']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '.') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = if elem '.' a then pure ((World EditX y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditX y u v w (a++['.']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '-') K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditX y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditX y u v w (a++['-']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (SpecialKey KeyBackspace) K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditX y u v w (init a) (Vertex ((stringToFloat (init a)),b2,b3)) c)) else pure ((World EditX y u v w a (Vertex (b1,b2,b3)) c))
-- text entry in edit y mode
inputHandlerFunction (EventKey (Char '0') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['0']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '1') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['1']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '2') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['2']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '3') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['3']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '4') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['4']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '5') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['5']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '6') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['6']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '7') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['7']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '8') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['8']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '9') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditY y u v w (a++['9']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '.') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = if elem '.' a then pure ((World EditY y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditY y u v w (a++['.']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '-') K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditY y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditY y u v w (a++['-']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (SpecialKey KeyBackspace) K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditY y u v w (init a) (Vertex ((stringToFloat (init a)),b2,b3)) c)) else pure ((World EditY y u v w a (Vertex (b1,b2,b3)) c))
-- text entry in edit z mode
inputHandlerFunction (EventKey (Char '0') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['0']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '1') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['1']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '2') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['2']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '3') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['3']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '4') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['4']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '5') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['5']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '6') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['6']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '7') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['7']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '8') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['8']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '9') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = pure ((World EditZ y u v w (a++['9']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '.') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = if elem '.' a then pure ((World EditZ y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditZ y u v w (a++['.']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (Char '-') K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditZ y u v w a (Vertex (b1,b2,b3)) c)) else pure ((World EditZ y u v w (a++['-']) (Vertex (b1,b2,b3)) c))
inputHandlerFunction (EventKey (SpecialKey KeyBackspace) K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = if (length a /= 0) then pure ((World EditZ y u v w (init a) (Vertex ((stringToFloat (init a)),b2,b3)) c)) else pure ((World EditZ y u v w a (Vertex (b1,b2,b3)) c))
-- exit program
inputHandlerFunction (EventKey (SpecialKey KeyEsc) K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = exitWith ExitSuccess
inputHandlerFunction (EventKey (SpecialKey KeyEsc) K.Down _ _) (World EditX y u v w a (Vertex (b1,b2,b3)) c) = exitWith ExitSuccess
inputHandlerFunction (EventKey (SpecialKey KeyEsc) K.Down _ _) (World EditY y u v w a (Vertex (b1,b2,b3)) c) = exitWith ExitSuccess
inputHandlerFunction (EventKey (SpecialKey KeyEsc) K.Down _ _) (World EditZ y u v w a (Vertex (b1,b2,b3)) c) = exitWith ExitSuccess
-- save model
inputHandlerFunction (EventKey (Char 's') K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = do
	(openFile "savefile.txt" WriteMode) >>= (\x -> (hPrint x (removeAxes y)) >> (hClose x))
	pure (World View y u v w a (Vertex (b1,b2,b3)) c)
-- start fresh
inputHandlerFunction (EventKey (SpecialKey KeyDelete) K.Down _ _) (World View y u v w a (Vertex (b1,b2,b3)) c) = pure (World View (map (\a -> rotationY (pi/2) a) (map (\a -> rotationX (-pi/2) a) axes)) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointX (1,0,0))))) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointY (0,1,0))))) (normalise ((\a -> rotationY (pi/2) a) ((\a -> rotationX (-pi/2) a) (AxisPointZ (0,0,1))))) a (Vertex (b1,b2,b3)) c)
-- bottom pattern handles the 'no-event' event
inputHandlerFunction e (World x y u v w a (Vertex (b1,b2,b3)) c) = pure ((World x y u v w a (Vertex (b1,b2,b3)) c))

----------------------------------------------------------------------------

-- world updater function

worldUpdaterFunction :: Float -> World -> IO World
worldUpdaterFunction = \t -> \(World x y u v w a (Vertex (b1,b2,b3)) c) -> case x of
	Title -> pure (World x y u v w a (Vertex (b1,b2,b3)) c)
	EditX -> pure (World x y u v w a (Vertex ((stringToFloat a),b2,b3)) c)
	EditY -> pure (World x y u v w a (Vertex (b1,(stringToFloat a),b3)) c)
	EditZ -> pure (World x y u v w a (Vertex (b1,b2,(stringToFloat a))) c)
	View -> pure (World x y u v w a (Vertex (b1,b2,b3)) c)
	
-- filters model of axes before saving (you end up with multiple axes on reboot otherwise)

removeAxes :: Obj -> Obj
removeAxes x = x \\ ([AxisPointX (a,b,c) | AxisPointX (a,b,c) <- x] ++ [AxisPointY (a,b,c) | AxisPointY (a,b,c) <- x] ++ [AxisPointZ (a,b,c) | AxisPointZ (a,b,c) <- x])

----------------------------------------------------------------------------

-- helper function to allow loading from file

readObjFromFile :: String -> Obj
readObjFromFile x = read x

main :: IO ()
main = do
	input <- readFile "savefile.txt"
	playIO windowInfo bgColor 180 (initWorld (readObjFromFile input)) renderFunction inputHandlerFunction worldUpdaterFunction


