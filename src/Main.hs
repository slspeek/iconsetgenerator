{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour.SRGB
import Diagrams.Core.Points
import Data.Colour (withOpacity)


favorite' :: Renderable (Path R2) b => Diagram b R2
favorite' =  stroke (star (StarSkip 2) (regPoly 5 1))

favorite = favorite'

crossy :: Renderable (Path R2) b => Double -> Diagram b R2
crossy a =  stroke ( fromVertices plusPath) # scale (8/9)
       where  plusPath =  concat . take 4 . iterate (rotateBy (-1/4)) $ spike
              b = 1 - a
              spike = map p2 [(-a, a), (-a,b), (a, b), (a, a)]

plusIcon :: Renderable (Path R2) b => Diagram b R2
plusIcon = crossy (1/5)

minusIcon :: Renderable (Path R2) b => Diagram b R2
minusIcon = rect (3/2) (1/2) # scale (3/4)

rightTriangle :: Renderable (Path R2) b => Diagram b R2
rightTriangle = eqTriangle 1 # rotateBy (3/4)

leftArrow :: (PathLike p, V p ~ R2) => p
leftArrow = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                   -1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac,
                                                    1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac ]
                                           [1/3 , 1 , a, eqSide, eqSide, a],
                           polyOrient = OrientV }
          where a = 1 /6
                eqSide =  2 * sqrt ( 1/3 *a)

enveloppe :: Renderable (Path R2) b => Colour Double -> Colour Double -> Diagram b R2
enveloppe themeFc themeLc =  style (mconcat $ map stroke pathList)
         where
             style = \y -> y # lineJoin LineJoinRound # lc themeLc # fc themeFc # centerXY # lw 0.03 # scale 1.2
             h = 3/4
             w = 1
             ltratio = 1/3
             lower_tip = p2 ( w/2, ltratio * h)
             upper_tip = p2 ( w/2, (1 - ltratio ) * h)
             lu_corner = p2  (0, h)
             ru_corner = p2 (w, h)
             lb_corner = p2 (0, 0)
             rb_corner = p2 (w, 0)
             envList = [lu_corner, ru_corner, rb_corner, lb_corner]
             flap = fromVertices [lu_corner, lower_tip, ru_corner]
             lowerFlap = fromVertices [lb_corner, upper_tip, rb_corner]
             env =   close (fromVertices envList)
             pathList = [ flap, lowerFlap, env]

strutedVrule :: Renderable (Path R2) b => Diagram b R2
strutedVrule = minusIcon # rotateBy (1/4) # scale 0.9

rightArrow :: Renderable (Path R2) b => Diagram b R2
rightArrow = leftArrow # reflectX

upArrow :: Renderable (Path R2) b => Diagram b R2
upArrow = rightArrow # rotateBy (1/4)

downArrow :: Renderable (Path R2) b => Diagram b R2
downArrow = upArrow # rotateBy (1/2)

play :: Renderable (Path R2) b => Diagram b R2
play = rightTriangle

stepNext :: Renderable (Path R2) b => Diagram b R2
stepNext = (play ||| strutedVrule) # alignX 0

stepPrevious :: Renderable (Path R2) b => Diagram b R2
stepPrevious = stepNext # reflectX

stepDown :: Renderable (Path R2) b => Diagram b R2
stepDown = stepNext # rotateBy (3/4)

stepUp :: Renderable (Path R2) b => Diagram b R2
stepUp = stepDown # reflectY

block :: Renderable (Path R2) b => Diagram b R2
block = square 1

fastForward :: Renderable (Path R2) b => Diagram b R2
fastForward = (rightTriangle # alignR ||| rightTriangle) # scale (3/4)

endIcon :: Renderable (Path R2) b => Diagram b R2
endIcon = (play ||| stepNext) # scale (3/4) # alignX 0

home ::  Renderable (Path R2) b => Diagram b R2
home = endIcon # reflectX

fastBackward :: Renderable (Path R2) b => Diagram b R2
fastBackward = fastForward # reflectX

innerCircle :: (PathLike p, Transformable p, V p ~ R2) => p
innerCircle =  circle 0.7

ring :: Path R2
ring = circle 1 <> innerCircle

coloredRing :: Renderable (Path R2) b => Colour Double -> Diagram b R2
coloredRing themeFc = stroke ring # fc themeFc # fillRule EvenOdd # lw 0

loop :: Renderable (Path R2) b => Colour Double -> Colour Double -> Diagram b R2
loop themeFc themeLc =  innerCircle # lc themeLc <> (arcPath <> (handlePath # fc themeFc))  <> coloredRing themeFc
       where a  = 1/40
             arcPath = arc (a :: CircleFrac) (-a :: CircleFrac)
             allArcPoints = concat (pathVertices arcPath)
             endP = head allArcPoints
             startP = last allArcPoints
             d = 1.5
             handleTop = endP # translateX d
             handleBottom = startP # translateX d
             handlePath = fromVertices [endP, handleTop, handleBottom, startP]

combineWithLoop :: Renderable (Path R2) b => Diagram b R2 -> Colour Double -> Colour Double -> Diagram b R2
combineWithLoop diagram themeFc themeLc = ((diagram # fc themeFc # rotateBy (3/8:: CircleFrac) # scale 0.8
                                                  <>
                                            loop themeFc themeLc) # rotateBy (-3/8:: CircleFrac) # lc themeLc)# centerXY # scale 0.5 


zoomIn :: Renderable (Path R2) b => Colour Double -> Colour Double -> Diagram b R2
zoomIn  =  combineWithLoop  plusIcon

zoomOut :: Renderable (Path R2) b => Colour Double -> Colour Double -> Diagram b R2
zoomOut  = combineWithLoop minusIcon

allIcons :: Renderable (Path R2) b => [([Char], Diagram b R2)]
allIcons = [ ( "right_arrow", rightArrow),
              ( "left_arrow", leftArrow),
              ( "up_arrow", upArrow),
              ( "down_arrow", downArrow),
              ( "next", stepNext),
              ( "previous", stepPrevious),
              ( "stepUp", stepUp),
              ( "stepDown", stepDown),
              ( "right_triangle", play),
              ( "stop", block),
              ( "home", home),
              ( "end", endIcon),
              ( "plus", plusIcon),
              ( "minus", minusIcon),
              ( "favorite", favorite),
              ( "fast_forward", fastForward),
              ( "rewind", fastBackward) ]

mapScd f names = map f' names
        where f' (x,y) = (x, f y)

hardList :: Renderable (Path R2) b => [([Char], Colour Double -> Colour Double -> Diagram b R2)]
hardList = [( "zoom_out", zoomOut ),
            ( "mail", enveloppe ),
            ( "zoom_in", zoomIn ) ]

hardToColor :: Renderable (Path R2) b =>  Colour Double -> Colour Double -> [([Char], Diagram b R2)]
hardToColor themeFc themeLc = mapScd (\y -> shadowed' y themeFc themeLc) hardList

 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
backgroundShape' :: (PathLike b, Transformable b, HasStyle b, V b ~ R2) =>
     Colour Double -> Colour Double -> b
backgroundShape' color lineC = circle 1 # fc color # lc lineC

shadowDirection = r2 (0.05, -0.05)
shadowed icon fC = centerXY (icon <> lc dC (fc dC (translate shadowDirection icon)))
        where dC = darken 0.3 fC
shadowed' icon fC lC = centerXY (icon fC lC <> translate shadowDirection (icon dC dC))
        where dC = darken 0.3 fC

setOnBackground fC bC lineC = map (padIcon 1.1)
        where padIcon  x (name, icon)  = (name , (icon <> backgroundShape' bC lineC) # lw 0.01 # pad x  )

setColorOnAll fillColor lineColor = mapScd (\y -> shadowed y fillColor # lc lineColor # fc fillColor)

prepareAll
  :: (Renderable (Path R2) b, Backend b R2) =>
     String -> String -> String -> [([Char], QDiagram b R2 Any)]
prepareAll fC bC lC =  ( "overview", overviewImage) : allPadded
        where   fcC = sRGB24read fC
                bgC = sRGB24read bC
                lcC = sRGB24read lC
                simpleIcons = setColorOnAll fcC lcC allIcons
                hardIcons = hardToColor fcC lcC
                allList = concat [simpleIcons, hardIcons]
                allPadded = setOnBackground fcC bgC lcC allList
                iconList = map snd allPadded
                overviewImage = hcat iconList

main :: IO ()
main = do
       fcString <- getLine
       bgString <- getLine
       lcString <- getLine
       multiMain  (prepareAll fcString bgString lcString)
