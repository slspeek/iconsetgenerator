{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Colour.SRGB
import Diagrams.Core.Points
import Data.Colour (withOpacity)
gear :: Renderable (Path R2) b => Double -> Diagram b R2
gear teeth =(stroke (fromVertices plusPath  <> circle 0.8)) # fillRule EvenOdd # scale 0.666666
       where  plusPath =  concat . take (floor teeth) . iterate (rotateBy (-angle)) $ spike
              angle = CircleFrac (1/teeth)
              a = pi / teeth 
              th = 1/4
              spike = map p2 [(-a, 1), (-(2/3) * a,1 + (th*th)), (-(1/3)* a, 1+th), (a/3, 1 + th), ((2/3)*a,1+(th^2)), (a,1)]

gearExample = centerXY $ beside (r2(1,1)) (scale 0.6 (gear 12)) (scale 0.4 (gear 8))

running :: Renderable (Path R2) b => Diagram b R2
running = centerXY $ scale (1/56) $ stroke $ path <> (circle 8 # translate (r2(73, 87)))
        where 
              path = close $ fromVertices points
              points = map  p2 [(54, 5),(64, 3),(70, 35),
                       (57, 49), (72,65),(82,56),(99,73),
                       (95,78),(83,67),(71,78),(65,80),
                       (43,80), (27,62), (32, 56), (47, 73),
                       (54,72),(26,39),(2, 39), (2, 29), 
                       (33,30),(45,44), (59,30)  ]


gradExample :: (PathLike a, Transformable a, HasStyle a, V a ~ R2) =>
    b -> b1 -> a
gradExample  = const $  const $ mconcat coloredCircles
        where circles = take count $ iterate (scale 0.99) (circle 1)
              colors = take count $ iterate (blend 0.01 blue) red
              coloredCircles = [ lc color circle| (color, circle) <- zip colors circles]
              count = 200

heart :: Renderable (Path R2) b => Diagram b R2
heart =  stroke (pathFromTrailAt heartT (p2(0,-2))) # scaleY 2 # scaleX 2.4 # centerXY
        where c1 = r2 (0.25, 0.2)
              c2 = r2 (0.5,0)
              c3 = r2 (0,-0.5)
              rightCurve = bezier3 c1 c2 c3
              leftCurve = rightCurve # reflectX
              heartT :: Trail R2
              heartT =  fromSegments [leftCurve , reverseSegment rightCurve]

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

leftArrow' headFactor = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                   -1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac,
                                                    1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac ]
                                           [1/3 , 1 , a, eqSide, eqSide, a],
                        polyOrient = OrientV }
          where a = headFactor
                eqSide =  2 * sqrt ( 1/3 *a)

leftArrow :: (PathLike p, V p ~ R2) => p
leftArrow = leftArrow' (1/6)

pencil w h pointL = centerXY $ stroke pencilPath # lineJoin LineJoinRound
    where leftCurveS = bezier3 (r2(b,0)) (r2(-b,-b)) (r2(0, h))
          b = h/4
          lineS = Linear (r2(w,0))
          pointS = Linear (r2(pointL, -(h/2)))
          rightCurveS = reverseSegment leftCurveS
          pencilPath = close $ fromSegments [leftCurveS, lineS, pointS, reverseSegment $ reflectY pointS]

pencilExample  = pencil 1.5 0.2 0.3 # rotateBy (-3/8)

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
play = hrule 0.2 # lw 0 ||| rightTriangle

stepNext :: Renderable (Path R2) b => Diagram b R2
stepNext = (rightTriangle ||| strutedVrule) # alignX 0

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
endIcon = (rightTriangle ||| stepNext) # scale (3/4) # alignX 0

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

allIcons = [  ("running", running),
              ("right_arrow", rightArrow),
              ("left_arrow", leftArrow),
              ("heart", heart),
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
              ("pencil", pencilExample),
              ("gear", gearExample),
              ("fast_forward", fastForward),
              ("rewind", fastBackward) ]

makeColorable
  :: HasStyle b => b -> Colour Double -> Colour Double -> b
makeColorable icon fC lC = icon # fc fC # lc lC

mapScd :: (t -> t2) -> [(t1, t)] -> [(t1, t2)]
mapScd f = map f'
        where f' (x,y) = (x, f y)

colorableList
  :: Renderable (Path R2) b =>
     [(String, Colour Double -> Colour Double -> Diagram b R2)]
colorableList = [( "zoom_out", zoomOut ),
                ( "mail", enveloppe ),
                ("gradExample", gradExample),
                ( "zoom_in", zoomIn ) ]

totalIconList
  :: Renderable (Path R2) b =>
     [(String, Colour Double -> Colour Double -> Diagram b R2)]
totalIconList = mapScd makeColorable allIcons ++ colorableList

themeIcons
  :: (Fractional a, Semigroup t2, Alignable t2, Transformable t2,
      ColourOps f, V t2 ~ R2) =>
     f a -> f a -> String -> [(t1, f a -> f a -> t2)] -> [(t1, t2)]
themeIcons themeFc themeLc shadow = mapScd (\y -> shadowedCond y themeFc themeLc shadow)

 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
backgroundShape' :: (PathLike b, Transformable b, HasStyle b, V b ~ R2) =>
     Colour Double -> Colour Double -> b
backgroundShape' color lineC = circle 1  # fc color # lw 0

shadowDirection :: R2
shadowDirection = r2 (0.05, -0.05)

shadowed'
  :: (Fractional a, Semigroup a1, Alignable a1, Transformable a1,
      ColourOps f, V a1 ~ R2) =>
     (f a -> f a -> a1) -> f a -> f a -> a1
shadowed' icon fC lC = centerX (icon fC lC <> translate shadowDirection (icon dC dC))
        where dC = darken 0.3 fC

shadowedCond
  :: (Fractional a, Semigroup a1, Alignable a1, Transformable a1,
      ColourOps f, V a1 ~ R2) =>
     (f a -> f a -> a1) -> f a -> f a -> String -> a1
shadowedCond icon fC lC s = if s == "True" then
                                shadowed' icon fC lC
                            else
                                icon fC lC # centerXY

setOnBackground ::  (PathLike t2, Transformable t2, HasStyle t2, V t2 ~ R2) =>
    Colour Double -> Colour Double -> [(t1, t2)] -> [(t1, t2)]
setOnBackground bC lC = mapScd (\icon -> icon <> backgroundShape' bC lC)

centerInCircle = mapScd (\icon -> icon <> (circle 1 # lw 0))

prepareAll ::  (Renderable (Path R2) b, Backend b R2) =>
    String -> String -> String -> String -> String -> [(String, QDiagram b R2 Any)]
prepareAll f b l shadow background =  ( "overview", overviewImage) : allPadded
        where   fC = sRGB24read f
                bC = sRGB24read b
                lC = sRGB24read l
                themedIcons = themeIcons fC lC shadow totalIconList
                onBackground = (if background == "True"  then
                                setOnBackground bC lC themedIcons
                            else
                                centerInCircle themedIcons)
                padList  = mapScd (pad 1.1)
                allPadded = padList onBackground
                noBG =  centerInCircle $ themeIcons fC lC "True" totalIconList
                noBGnoSh =  centerInCircle $ themeIcons fC lC "False" totalIconList
                noShadow = setOnBackground bC lC $ themeIcons fC lC "False" totalIconList
                overviewImage = vcat $ map hcat $ (map (map snd)) $ map padList [noBGnoSh,noBG, noShadow, onBackground]


main :: IO ()
main = do
       fcString <- getLine
       bgString <- getLine
       lcString <- getLine
       shadow <- getLine
       background <- getLine
       multiMain  (prepareAll fcString bgString lcString shadow background)
