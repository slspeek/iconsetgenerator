{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module IconsetGenerator.Icons (
                  allIcons
                , backgroundShape'
                , block
                , centerInCircle
                , colorableList
                , combineWithLoop
                , crossy
                , door
                , downArrow
                , endIcon
                , enveloppe
                , fastBackward
                , fastForward
                , favorite
                , favorite'
                , gear
                , gearExample
                , gradExample
                , heart
                , helpIcon
                , home
                , info
                , key
                , leave
                , leftArrow
                , leftArrow'
                , loop
                , makeColorable
                , mapScd
                , minusIcon
                , pause
                , pencil
                , pencilExample
                , play
                , plusIcon
                , prepareAll
                , radioWaves
                , rightArrow
                , rightTriangle
                , rssIcon
                , running
                , setOnBackground
                , shadowDirection
                , shadowed'
                , shadowedCond
                , stepDown
                , stepNext
                , stepPrevious
                , stepUp
                , verticalBar
                , textIcon
                , totalIconList
                , upArrow
                , zoomIn
                , zoomOut
                                ) where

import           Data.Colour          (withOpacity)
import           Data.Colour.SRGB
import           Diagrams.Core.Points
import           Diagrams.Prelude
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Text   (Text)


textIcon :: Renderable Diagrams.TwoD.Text.Text b => String -> Diagram b R2
textIcon txt = scale 1.6 $ font "Serif" $ italic $ text txt

helpIcon ::  Renderable Diagrams.TwoD.Text.Text b => Diagram b R2
helpIcon = textIcon "?"

info ::  Renderable Diagrams.TwoD.Text.Text b => Diagram b R2
info = textIcon "i"

userGroup :: (PathLike a, Alignable a, Transformable a, HasStyle a,Juxtaposable a, V a ~ R2) =>Colour Double -> t -> a
userGroup mC lC = centerXY $ user1 <> user2
        where   user1 = user # scale 0.9 # fc mC # lc mC
                user2 = user # scale 0.85 # translate (r2(0.5,0.10)) # fc (darken 0.8 mC) # lc (darken 0.8 mC)

user :: (PathLike t, Alignable t, Transformable t, Juxtaposable t,V t ~ R2) =>t
user = scale 1.3 $ centerXY $ (circle 0.2) === roundedRect' 0.7 0.6 with { radiusTL = 0.2
                                        , radiusTR = 0.2}
leave :: Renderable (Path R2) b =>Colour Double -> Colour Double -> Diagram b R2
leave fC lC = fc fC $ lc lC $ translateX (-0.1) $ centerXY $ scale 0.7 $ arrow <> door fC lC
        where   arrow = rightArrow #  scale 0.8 # translate (r2(-0.9,0))

door :: Renderable (Path R2) b => Colour Double -> t -> Diagram b R2
door fC lC = (openDoor <> doorFrame # translate (r2(x/2 -d , -y/2 - a))) # centerXY
        where   doorFrame = stroke (rect x y) # fcA transparent # lw 0.03
                d = 0.2
                a = 0.03
                x = 1
                y = -1.7
                doorOpenPath = fromOffsets [r2(x-d, -a), r2(0, -y), r2(-x+d, -a)]
                doorClosed = close doorOpenPath
                doorKnob = stroke (circle 0.04) # fc (darken 0.1 fC) # translate (r2(d/2, -y/2 -a))
                openDoor = doorKnob <> stroke doorClosed


radioWaves :: (Angle a, PathLike b, HasStyle b, V b ~ R2) => a -> Double -> b
radioWaves a n = mconcat waves # fcA transparent
       where   waves = [ arc' r 0 a | r <- [(1/n),(2/n)..1]]


rssIcon :: (PathLike b, Color c, Transformable b, HasStyle b, V b ~ R2) =>c -> t -> b
rssIcon mainColor lC = (circle 0.08 <> radioWaves (1/4::CircleFrac)  3) # lineCap LineCapRound # lw 0.1 # translate (-r2(0.4,0.4)) # lineColor mainColor # fillColor mainColor


key ::  Renderable (Path R2) b => Diagram b R2
key =  stroke (innerCircle <> (handlePath # moveOriginBy (r2(-(d+1.6),0)))) # fillRule EvenOdd # scale 0.40 # centerXY
       where a  = 1/20
             innerCircle =  circle 0.3
             arcTrail = arcT (a :: CircleFrac) (-a :: CircleFrac)
             handleTopLineT = fromOffsets [r2(-b,b), r2(-d,0)]
             handleBottomLineT = fromOffsets $ [r2(d-2*c*b-2*b,0), r2(b,b)] ++ mconcat (replicate c [r2(b,-b),r2(b,b)]) ++ [r2(b,0)]
             c = 3
             b= 0.23
             fullTrail :: Trail R2
             fullTrail = mconcat [handleTopLineT ,arcTrail, handleBottomLineT]
             d = 2
             handlePath = close $ pathFromTrail fullTrail


gear :: Renderable (Path R2) b => Double -> Diagram b R2
gear teeth = stroke (fromVertices plusPath  <> circle 0.8) # fillRule EvenOdd # scale 0.666666
       where  plusPath =  concat . take (floor teeth) . iterate (rotateBy (-angle)) $ spike
              angle = CircleFrac (1/teeth)
              a = pi / teeth
              th = 1/4
              spike = map p2 [(-a, 1), (-(2/3) * a,1 + (th*th)), (-(1/3)* a, 1+th), (a/3, 1 + th), ((2/3)*a,1+(th^2)), (a,1)]


gearExample ::  Renderable (Path R2) b => Diagram b R2
gearExample = centerXY $ beside (r2(1,1)) (scale 0.6 (gear 12)) (scale 0.4 (gear 8))


running :: Renderable (Path R2) b => Diagram b R2
running = translateX (-0.02) $ centerXY $ scale (1/56) $ stroke $ path <> (circle 8 # translate (r2(73, 87)))
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
heart =  stroke (pathFromTrailAt heartT (p2(0,-2))) # scaleY 2 # scaleX 2.4 # centerXY # translateY (-0.12)
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

pause = translateX (-0.3) verticalPole ||| strutX 0.2 ||| translateX 0.3 verticalPole
        where   verticalPole = verticalBar

rightTriangle :: Renderable (Path R2) b => Diagram b R2
rightTriangle = eqTriangle 1 # rotateBy (3/4)


leftArrow' ::  (PathLike p, V p ~ R2) => Double -> p
leftArrow' headFactor = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                   -1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac,
                                                    1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac ]
                                           [1/3 , 1 , a, eqSide, eqSide, a],
                        polyOrient = OrientV }
          where a = headFactor
                eqSide =  2 * sqrt ( 1/3 *a)


leftArrow ::  (PathLike b, Alignable b, V b ~ R2) => b
leftArrow = leftArrow' (1/6) # centerXY


pencil :: Renderable (Path R2) b =>Double -> Double -> Double -> Diagram b R2
pencil w h pointL = centerXY $ stroke pencilPath # lineJoin LineJoinRound
    where leftCurveS = bezier3 (r2(b,0)) (r2(-b,-b)) (r2(0, h))
          b = h/4
          lineS = Linear (r2(w,0))
          pointS = Linear (r2(pointL, -(h/2)))
          rightCurveS = reverseSegment leftCurveS
          pencilPath = close $ fromSegments [leftCurveS, lineS, pointS, reverseSegment $ reflectY pointS]


pencilExample ::  Renderable (Path R2) b => Diagram b R2
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


verticalBar :: Renderable (Path R2) b => Diagram b R2
verticalBar = minusIcon # rotateBy (1/4) # scale 0.9


rightArrow :: Renderable (Path R2) b => Diagram b R2
rightArrow = leftArrow # reflectX


upArrow :: Renderable (Path R2) b => Diagram b R2
upArrow = rightArrow # rotateBy (1/4)


downArrow :: Renderable (Path R2) b => Diagram b R2
downArrow = upArrow # rotateBy (1/2)

play :: Renderable (Path R2) b => Diagram b R2
play = (hrule 0.2 # lw 0 ||| rightTriangle) # centerXY

stepNext :: Renderable (Path R2) b => Diagram b R2
stepNext = (rightTriangle ||| verticalBar) # alignX 0

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

loop ::  Renderable (Path R2) b => Diagram b R2
loop =  stroke (innerCircle <> (handlePath # moveOriginTo endP)) # fillRule EvenOdd
       where a  = 1/40
             innerCircle =  circle 0.7
             arcTrail = arcT (a :: CircleFrac) (-a :: CircleFrac)
             handleTopLineT = fromOffsets [r2(-d,0)]
             handleBottomLineT = fromOffsets [r2(d,0)]
             fullTrail :: Trail R2
             fullTrail = mconcat [handleTopLineT ,arcTrail, handleBottomLineT]
             d = 1.5
             handlePath = close $ pathFromTrail fullTrail
             allPoints = concat (pathVertices handlePath)
             endP = last allPoints # scale 0.5 # translate (r2(-(d+1),0))

combineWithLoop :: Renderable (Path R2) b => Diagram b R2 -> Diagram b R2
combineWithLoop diagram = ((diagram # rotateBy (3/8:: CircleFrac) # scale 0.8
                                                  <>
                                            loop) # rotateBy (-3/8:: CircleFrac))# centerXY # scale 0.5


zoomIn ::  Renderable (Path R2) b => Diagram b R2
zoomIn  =  combineWithLoop  plusIcon

zoomOut ::  Renderable (Path R2) b => Diagram b R2
zoomOut  = combineWithLoop minusIcon

allIcons :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>[([Char], Diagram b R2)]
allIcons = [  ("running", running),
              ("gear", gearExample),
              ("key", key),
              ("heart", heart),
              ("favorite", favorite),
              ("pencil", pencilExample),
              ("right_arrow", rightArrow),
              ("left_arrow", leftArrow),
              ("up_arrow", upArrow),
              ("down_arrow", downArrow),
              ("next", stepNext),
              ("previous", stepPrevious),
              ("step_up", stepUp),
              ("step_down", stepDown),
              ("pause", pause),
              ("right_triangle", play),
              ("stop", block),
              ("home", home),
              ("end", endIcon),
              ("plus", plusIcon),
              ("minus", minusIcon),
              ("zoom_out", zoomOut ),
              ("zoom_in" ,zoomIn),
              ("user", user),
              ("info", info),
              ("help", helpIcon),
              ("fast_forward", fastForward),
              ("rewind", fastBackward) ]

makeColorable :: HasStyle b => b -> Colour Double -> Colour Double -> b
makeColorable icon fC lC = icon # fc fC # lc lC

mapScd :: (t -> t2) -> [(t1, t)] -> [(t1, t2)]
mapScd f = map f'
        where f' (x,y) = (x, f y)

colorableList :: Renderable (Path R2) b =>[([Char], Colour Double -> Colour Double -> Diagram b R2)]
colorableList = [
            ( "mail", enveloppe ),
            ("rss", rssIcon),
            ("userGroup", userGroup),
            ("leave", leave)
              --  ("gradExample", gradExample)
                ]

totalIconList :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) =>[([Char], Colour Double -> Colour Double -> Diagram b R2)]
totalIconList = mapScd makeColorable allIcons ++ colorableList


 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
backgroundShape' :: (PathLike b, Transformable b, HasStyle b, V b ~ R2) =>
     Colour Double -> Colour Double -> b
backgroundShape' color lineC = circle 1  # fc color # lw 0

shadowDirection ::  R2
shadowDirection = r2 (0.05, -0.05)

shadowed' :: (Fractional a, ColourOps f, Semigroup a1, Transformable a1,V a1 ~ R2) =>(f a -> f a -> a1) -> f a -> f a -> a1
shadowed' icon fC lC = icon fC lC <> translate shadowDirection (icon dC dC)
        where dC = darken 0.3 fC

shadowedCond :: (Fractional a, ColourOps f, Semigroup a1, Transformable a1,V a1 ~ R2) =>(f a -> f a -> a1) -> f a -> f a -> Bool -> a1
shadowedCond icon fC lC s = if s then
                                shadowed' icon fC lC
                            else
                                icon fC lC

applyTheming :: (Fractional a, ColourOps f, Semigroup t2, Transformable t2,V t2 ~ R2) =>f a -> f a -> Bool -> [(t1, f a -> f a -> t2)] -> [(t1, t2)]
applyTheming themeFc themeLc shadow = mapScd (\y -> shadowedCond y themeFc themeLc shadow)

setOnBackground ::  (PathLike t2, Transformable t2, HasStyle t2, V t2 ~ R2) =>
    Colour Double -> Colour Double -> [(t1, t2)] -> [(t1, t2)]
setOnBackground bC lC = mapScd (\icon -> icon <> backgroundShape' bC lC)

centerInCircle :: (PathLike t2, Transformable t2, HasStyle t2, V t2 ~ R2) =>[(t1, t2)] -> [(t1, t2)]
centerInCircle = mapScd (\icon -> icon <> (circle 1 # lw 0))

prepareAll :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>[Char]-> [Char]-> [Char]-> Bool-> Bool-> [([Char], QDiagram b R2 Any)]
prepareAll f b l shadow background =  ( "overview", overviewImage) : allPadded
        where   toColor x = sRGB24read ('#' : x)
                fC = toColor f
                bC = toColor b
                lC = toColor l
                themedIcons = applyTheming fC lC shadow totalIconList
                onBackground = if background then
                                setOnBackground bC lC themedIcons
                            else
                                centerInCircle themedIcons
                padList  = mapScd (pad 1.1)
                allPadded = padList onBackground
                noBG =  centerInCircle $ applyTheming fC lC True totalIconList
                noBGnoSh =  centerInCircle $ applyTheming fC lC False totalIconList
                noShadow = setOnBackground bC lC $ applyTheming fC lC False totalIconList
                overviewImage = vcat $ map (hcat . map snd . padList) [noBGnoSh, noBG, noShadow, onBackground]
