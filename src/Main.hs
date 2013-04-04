{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour.SRGB
import Diagrams.Core.Points
import Data.Colour (withOpacity)


hart :: Renderable (Path R2) b => Diagram b R2
hart =  stroke (pathFromTrailAt hartT (p2(0,-2))) # scaleY 2 # scaleX 2.4 # centerXY
        where c1 = r2 (0.25, 0.2)
              c2 = r2 (0.5,0)
              c3 = r2 (0,-0.5)
              rightCurve = bezier3 c1 c2 c3
              leftCurve = rightCurve # reflectX
              hartT :: Trail R2
              hartT =  fromSegments [leftCurve , reverseSegment rightCurve]

favorite' :: Renderable (Path R2) b => Diagram b R2
favorite' =  stroke (star (StarSkip 2) (regPoly 5 1))

favorite :: Renderable (Path R2) b => Diagram b R2
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
             style y   = y # lineJoin LineJoinRound # lc themeLc # fc themeFc # centerXY # lw 0.03 # scale 1.2
             h         = 3/4
             w         = 1
             ltratio   = 1/3
             lower_tip = p2 ( w/2, ltratio * h)
             upper_tip = p2 ( w/2, (1 - ltratio ) * h)
             lu_corner = p2  (0, h)
             ru_corner = p2 (w, h)
             lb_corner = p2 (0, 0)
             rb_corner = p2 (w, 0)
             envList   = [lu_corner, ru_corner, rb_corner, lb_corner]
             flap      = fromVertices [lu_corner, lower_tip, ru_corner]
             lowerFlap = fromVertices [lb_corner, upper_tip, rb_corner]
             env       =   close (fromVertices envList)
             pathList  = [ flap, lowerFlap, env]

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

allIcons :: Renderable (Path R2) b => [(String, Diagram b R2)]
allIcons = [  ("right_arrow", rightArrow),
              ("left_arrow", leftArrow),
              ("hart", hart),
              ("up_arrow", upArrow),
              ("down_arrow", downArrow),
              ("next", stepNext),
              ("previous", stepPrevious),
              ("stepUp", stepUp),
              ("stepDown", stepDown),
              ("right_triangle", play),
              ("stop", block),
              ("home", home),
              ("end", endIcon),
              ("plus", plusIcon),
              ("minus", minusIcon),
              ("favorite", favorite),
              ("fast_forward", fastForward),
              ("rewind", fastBackward) ]

mapScd :: (t -> t2) -> [(t1, t)] -> [(t1, t2)]
mapScd f = map f'
        where f' (x,y) = (x, f y)

hardList :: Renderable (Path R2) b => [(String, Colour Double -> Colour Double -> Diagram b R2)]
hardList = [( "zoom_out", zoomOut ),
            ( "mail", enveloppe ),
            ( "zoom_in", zoomIn ) ]

hardToColor :: Renderable (Path R2) b =>  Colour Double -> Colour Double -> [(String, Diagram b R2)]
hardToColor themeFc themeLc = mapScd (\y -> shadowed' y themeFc themeLc) hardList

 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
backgroundShape' :: (PathLike b, Transformable b, HasStyle b, V b ~ R2) =>
     Colour Double -> Colour Double -> b
backgroundShape' color lineC = circle 1 # fc color # lc lineC

shadowDirection :: R2
shadowDirection = r2 (0.05, -0.05)

shadowed :: (Semigroup a, Alignable a, Transformable a, HasStyle a, V a ~ R2) =>
    a -> Colour Double -> a
shadowed icon fC = centerX (icon <> lc dC (fc dC (translate shadowDirection icon)))
        where dC = darken 0.3 fC


shadowed' icon fC lC = centerX (icon fC lC <> translate shadowDirection (icon dC dC))
        where dC = darken 0.3 fC

setOnBackground :: (Semigroup m, PathLike (QDiagram b R2 m), Backend b R2,
                                   Monoid m) =>
    Colour Double -> Colour Double -> [(t1, QDiagram b R2 m)] -> [(t1, QDiagram b R2 m)]
setOnBackground bC lC = mapScd (\icon -> (icon <> backgroundShape' bC lC) # lw 0.01 # pad 1.1)

setColorOnAll :: (Semigroup t2, Alignable t2, Transformable t2, HasStyle t2, V t2 ~ R2) =>
    Colour Double -> Colour Double -> [(t1, t2)] -> [(t1, t2)]
setColorOnAll fC lC = mapScd (\y -> shadowed y fC # lc lC # fc fC)

prepareAll
  :: (Renderable (Path R2) b, Backend b R2) =>
     String -> String -> String -> [(String, QDiagram b R2 Any)]
prepareAll f b l =  ( "overview", overviewImage) : allPadded
        where   fC = sRGB24read f
                bC = sRGB24read b
                lC = sRGB24read l
                simpleIcons = setColorOnAll fC lC allIcons
                hardIcons = hardToColor fC lC
                allList = simpleIcons ++ hardIcons
                allPadded = setOnBackground bC lC allList
                iconList = map snd allPadded
                overviewImage = hcat iconList

main :: IO ()
main = do
       fcString <- getLine
       bgString <- getLine
       lcString <- getLine
       multiMain  (prepareAll fcString bgString lcString)
