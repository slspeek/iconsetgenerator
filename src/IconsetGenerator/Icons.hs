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
                , flower
                , gear
                , gearExample
                , gradExample
                , hare
                , treeIcon
                , haskell
                , heart
                , helpIcon
                , home
                , iconNames
                , info
                , key
                , leave
                , leftArrow
                , leftArrow'
                , locked
                , loop
                , makeColorable
                , mapScd
                , minusIcon
                , moon
                , mountains
                , pause
                , pencil
                , pencilExample
                , photo
                , play
                , plusIcon
                , prepareAll
                , radioWaves
                , reload
                , reloadTree
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
                , sun
                , switchOff
                , turtle
                , verticalBar
                , textIcon
                , totalIconList
                , upArrow
                , unlocked
                , zoomIn
                , zoomOut
                                ) where

import           Data.Colour.SRGB
import           Diagrams.Prelude
import           Diagrams.TwoD.Arc
import           Diagrams.TwoD.Text   (Text)

type DichromeIcon b = Colour Double -> Colour Double -> Diagram b R2

photo :: (Semigroup m, PathLike (QDiagram b R2 m), Monoid m) =>QDiagram b R2 m
photo = scale 0.75 ( freeze (roundedRect 2 1.5  0.2 # fcA transparent # lw 0.03
                     <> centerXY ( mountains <> (translate (r2 (0.6,0.5)) ( scale (1/3) sun)))))

mountains :: (PathLike a, Alignable a, Transformable a, V a ~ R2) => a
mountains = centerXY $ biggerMountain <> smallerMountain
    where   biggerMountain = eqTriangle 1.1 # alignB
            smallerMountain = scale (1-d) biggerMountain # translateX (2 * d) 
            d = 0.2

haskell :: Renderable (Path R2) b => Colour Double -> t -> Diagram b R2
haskell mC _ = scale 1.3 . centerXY $ (greaterThen mC # translateX (-w-d)) <> lambda <> pEqual <> pBEqual
    where   greaterThen mainColor = toPath [(w, 0), (gtX, -h/2), (-gtX, -h/2), (-w, 0), (gtX, h/2)] # fc mC # lc mainColor
            gtX = 0.25
            d = w/3
            darkerColor = darken 0.3 mC
            lambda = (toPath lVectors <> greaterThen darkerColor) # fc darkerColor # lc darkerColor
            lVectors = [(w, 0), (2*gtX, -h), (-w, 0) ]
            w = 0.2
            h = 0.8
            toPath = stroke . close . fromOffsets . map r2
            grR = gtX / (h/2)
            eh = 0.75 * w
            uEqual = toPath [(gtX +w - grR * eh,0),  (0, eh), (-gtX  - w, 0)] # fc mC # lc mC
            bEqual = toPath [(gtX + w - grR * (eh + d), 0), (0, -eh), (-gtX - w + grR * (2*eh + d), 0)]# fc mC # lc mC
            pEqual = translate (r2((gtX + w + (2/3)*d), -(h/2 - d/2))) uEqual
            pBEqual = translate (r2((gtX + w + (2/3)*d+ grR *(d)), -(h/2 + d/2))) bEqual

flower ::  (PathLike a, Transformable a, V a ~ R2) => a
flower = fCenter <> mconcat leafs
    where   fCenter = circle 0.2
            leafs = iterateN count (rotateBy ((1/count) :: CircleFrac)) leaf
            count = 11
            leaf = ellipse 0.5 # scale 0.3 # translateY 0.5 # scaleX (4/count)

sun ::  (PathLike a, Transformable a, HasStyle a, V a ~ R2) => a
sun = circle sun_dia <> mconcat sparkles
    where   sparkles = iterateN count (rotateBy ((1/count) :: CircleFrac)) line
            count = 11
            sun_dia = 0.4
            sp_len = sun_dia*0.60;
            line = vrule sp_len # translateY (0.3 + sun_dia) # lw (0.8/count) # lineCap LineCapRound

moon ::  Renderable (Path R2) b => Diagram b R2
moon = stroke $ centerXY $ rotateBy (-3/8 :: CircleFrac) $ bow 
  where   inner = scale moon_size $ arcT (0::CircleFrac) (0.5::CircleFrac)
          bow = pathFromTrail (mconcat [inner, terminator])
          moon_size = 0.8
          c1 = r2 (moon_size, 0.2)
          c3 = r2 (1.6, 0)
          rightCurve = bezier3 c1 c1 c3
          terminator = fromSegments [rightCurve]


locked ::  Renderable (Path R2) b => Diagram b R2
locked = centerXY . scale 0.7 $ (translateX (0.5) bow 
           ===
        roundedRect 1.3 1 0.04 )
        where   bow = lw 0.10 . fcA transparent . stroke . scale 0.5 . pathFromTrail
                 $ (vertT1 `mappend` arcT (0::CircleFrac) (1/2::CircleFrac)
                 `mappend` vertT2)
                        where   vertL  = 0.3
                                vertT1 = fromOffsets [r2(0,vertL)]
                                vertT2 = reflectY vertT1
                        
unlocked ::  Renderable (Path R2) b => Diagram b R2
unlocked = centerXY . scale 0.7 $ (translateX (-0.5) bow 
           ===
        roundedRect 1.3 1 0.04 )
        where   bow = lw 0.10 . fcA transparent . stroke . scale 0.5 . pathFromTrail
                 $ (vertT1 `mappend` arcT (0::CircleFrac) (1/2::CircleFrac)
                 `mappend` vertT2)
                        where   vertL  = 0.5
                                vertT1 = fromOffsets [r2(0,vertL)]
                                vertT2 = reflectY vertT1
          

turtle ::  (PathLike c, Alignable c, Transformable c, V c ~ R2) => c
turtle = translateX 0.02 . scale (1/400) . centerXY . reflectX . reflectY $ turtleSpline
        where   turtleHead :: [P2]
                turtleHead = [114&180,  76&161, 40&132, 18&119, 14&109, 3&105, 2&90, 8&66, 31&59, 76&70,
                        111&90, 115&88, 144&102]
                back = [169&86, 194&61, 216&48, 256&30, 321&13, 341&12, 350&14, 386&10, 418&11, 
                        438&16, 493&36, 548&84, 582&145, 592&174, 604&184, 582&219 ]
                rearLegs       = [552&234, 571&259, 601&291, 560&317, 520&288, 509&266]
                belly          = [376&270, 290&266, 200&268, 156&252]
                frontLegs      = [157&264, 146&297, 114&308, 84&294, 104&266, 91&208, 105&185]
                turtleShape         = concat [turtleHead, back, rearLegs, belly, frontLegs]
                turtleSpline   = cubicSpline True turtleShape


hare ::  (PathLike c, Alignable c, Transformable c, V c ~ R2) => c
hare = translateX 0.02 . scale (1/196) . centerXY . reflectX . reflectY $ hareSpline
        where   hareHead :: [P2]
                hareHead = [59&84.5, 51&74.5, 40&71, 40&61.5, 55.5&45, 65.5&38.5, 62&17, 67&5, 71.5&3.5,
                        73.5&4.5, 79.5&0, 84&1.5, 85.5&17, 84&30, 80.5&38, 82&48.5, 89&56, 94.5&60]
                back = [107.5&63, 155.5&70.5, 191&69.5, 216&63.5, 234.5&68.5, 246&79, 252.5&97.5,
                        257&108.5]
                rearLegs = [274.5&121.5, 300&134, 325&151.5, 331.5&160, 329&169, 312&162.5, 277.5&142,
                        251&132.5, 228&125.5, 209&117]
                belly = [191&117, 159&126, 121&127, 85&123, 75.5&123.5]
                frontLegs = [46.5&144, 20.5&157.5, 9.5&149, 30.5&143.5, 58.5&122.5, 30.5&117.5,
                        7.5&118, 0&111, 32.5&108.5, 56.5&105, 55.5&91.5]
                hareShape = concat [hareHead, back, rearLegs, belly, frontLegs]
                hareSpline = cubicSpline True hareShape

reloadTree :: (Renderable (Path R2) b, Backend b R2) =>Colour Double -> t -> QDiagram b R2 Any
reloadTree mC lC = translateX 0.1 . scale 0.8 . centerXY $ ((freeze $ treeIcon mC lC) ||| strutX 0.15 ||| (scale (4/7) $ freeze $ reflectX $ reload mC lC))

treeIcon :: Renderable (Path R2) b => Colour Double -> t -> Diagram b R2
treeIcon mC _ = scale 0.6 . centerXY . lw 0.13 . lc mC . stroke $ mconcat tree
        where   raT   = fromOffsets $ map r2 [(0,-1),(1.25,0)]
                raP1  = pathFromTrailAt raT (p2(-1,1))
                raP2  = pathFromTrailAt raT (p2(-1,0))
                raP3  = pathFromTrailAt (scaleY 0.5 raT) (p2(-1,1))
                raSP1 = pathFromTrailAt (scale 0.5 raT) (p2(-0.5, 0))
                raSP2 = pathFromTrailAt (scale 0.5 raT) (p2(-0.5, -1))
                tree  = [raP1, raP2, raP3, raSP1, raSP2]

defaultLineWidth ::  HasStyle a => a -> a
defaultLineWidth = lw 0.2

reload :: Renderable (Path R2) b => Colour Double -> t -> Diagram b R2
reload mC _ =  scale 0.7 . centerXY $ (stroke (handlePath )# defaultLineWidth <> arrowHead # fc mC) # lc mC
       where a                     = 1/4
             arcTrail              = arcT (0 :: CircleFrac) (-a :: CircleFrac)
             arrowHead             = eqTriangle d #  reflectY # translateY (-0.25 * d)
             fullTrail :: Trail R2
             fullTrail             = arcTrail
             d                     = 0.7
             handlePath            = pathFromTrail fullTrail

switchOff :: (PathLike a, Alignable a, Transformable a, HasStyle a, V a ~ R2) =>Colour Double -> t -> a
switchOff mC _ = lineCap LineCapRound . centerXY . scale 0.7 . lc mC . defaultLineWidth . centerXY $ arc ((1/4)+a :: CircleFrac) (1/4 - a :: CircleFrac)
                <>
                vrule 1 # translateY 0.6
        where   a = 1/13

textIcon :: Renderable Diagrams.TwoD.Text.Text b => String -> Diagram b R2
textIcon txt = scale 1.6 $ text txt

helpIcon ::  Renderable Diagrams.TwoD.Text.Text b => Diagram b R2
helpIcon = textIcon "?"

info ::  Renderable Diagrams.TwoD.Text.Text b => Diagram b R2
info = font "Serif" $ italic $ textIcon "i"

userGroup :: Renderable (Path R2) b => DichromeIcon b
userGroup mC lC = centerXY $ user1 <> user2
        where   user1 = user mC lC # scale 0.9
                user2 = user (darken 0.8 mC)  (darken 0.8 mC) # scale 0.85 # translate (r2(0.5,0.10))

user :: Renderable (Path R2) b => DichromeIcon b
user = makeColorable $ scale 1.3 $ centerXY $ (circle 0.2) === roundedRect' 0.7 0.6 with { radiusTL = 0.2
                                        , radiusTR = 0.2}

leave :: Renderable (Path R2) b =>Colour Double -> Colour Double -> Diagram b R2
leave fC lC = fc fC . lc lC . translateX (-0.1) . centerXY . scale 0.7 $ arrow <> door fC lC
        where   arrow = rightArrow #  scale 0.8 # translate (r2(-0.9,0))

door :: Renderable (Path R2) b => Colour Double -> t -> Diagram b R2
door fC _ = (openDoor <> doorFrame # translate (r2(x/2 -d , -y/2 - a))) # centerXY
        where   doorFrame    = stroke (rect x y) # fcA transparent # lw 0.03
                d            = 0.2
                a            = 0.03
                x            = 1
                y            = -1.7
                doorOpenPath = fromOffsets [r2(x-d, -a), r2(0, -y), r2(-x+d, -a)]
                doorClosed   = close doorOpenPath
                doorKnob     = stroke (circle 0.04) # fc (darken 0.1 fC) # translate (r2(d/2, -y/2 -a))
                openDoor     = doorKnob <> stroke doorClosed


radioWaves :: (Angle a, PathLike b, HasStyle b, V b ~ R2) => a -> Double -> b
radioWaves a n = mconcat waves # fcA transparent
       where   waves = [ arc' r 0 a | r <- [(1/n),(2/n)..1]]


rssIcon :: (PathLike b, Color c, Transformable b, HasStyle b, V b ~ R2) =>c -> t -> b
rssIcon mainColor _ = (circle 0.08 <> radioWaves (1/4::CircleFrac)  3) # lineCap LineCapRound # lw 0.1 # translate (-r2(0.4,0.4)) # lineColor mainColor # fillColor mainColor


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
              spike = map p2 [(-a, 1), (-(2/3) * a,1 + (th*th)), (-(1/3)* a, 1+th), (a/3, 1 + th), ((2/3)*a,1+(th*th)), (a,1)]


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
              coloredCircles = [ lc color aCircle| (color, aCircle) <- zip colors circles]
              count = 200


heart ::  Renderable (Path R2) b => Diagram b R2
heart = translateY (-0.10) $ centerY $ stroke (pathFromTrailAt heartT (p2(0,-2))) # scaleY 2 # scaleX 2.4 
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

pause ::  (Renderable (Path R2) b, Backend b R2) => Diagram b R2
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
loop =   stroke (innerCircle <> (handlePath # moveOriginTo endP)) # fillRule EvenOdd
       where a  = 1/50
             innerCircle =  circle 0.8
             arcTrail = arcT (a :: CircleFrac) (-a :: CircleFrac)
             handleTopLineT = fromOffsets [r2(-d,0)]
             handleBottomLineT = fromOffsets [r2(d,0)]
             fullTrail :: Trail R2
             fullTrail = mconcat [handleTopLineT ,arcTrail, handleBottomLineT]
             d = 0.7
             handlePath = close $ pathFromTrail fullTrail
             allPoints = concat (pathVertices handlePath)
             endP = last allPoints # scale 0.5 # translate (r2(-(d+1),0))

combineWithLoop :: Renderable (Path R2) b => Diagram b R2 -> Diagram b R2
combineWithLoop diagram =  positionDiagram ( positionSubdiagram diagram
                                                  <>
                                            loop)
          where   positionDiagram = translate (r2(0.07, 0.07)) . scale 0.7 . centerXY . rotateBy (-3/8:: CircleFrac)
                  positionSubdiagram = rotateBy (3/8:: CircleFrac) . scale 0.8

zoomIn ::  Renderable (Path R2) b => Diagram b R2
zoomIn  =  combineWithLoop  plusIcon

zoomOut ::  Renderable (Path R2) b => Diagram b R2
zoomOut  = combineWithLoop minusIcon

allIcons :: (Renderable Text b, Renderable (Path R2) b, Backend b R2) =>[([Char], Diagram b R2)]
allIcons = [  ("running", running),
              ("hare", hare),
              ("mountains", mountains),
              ("sun", sun),
              ("moon", moon),
              ("flower", flower),
              ("locked", locked),
              ("unlocked", unlocked),
              ("turtle", turtle),
              ("gear", gearExample),
              ("key", key),
              ("heart", heart),
              ("favorite", favorite),
              ("pencil", pencilExample),
              ("photo", photo),
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
              ("info", info),
              ("help", helpIcon),
              ("fast_forward", fastForward),
              ("rewind", fastBackward) ]

makeColorable :: HasStyle b => b -> Colour Double -> Colour Double -> b
makeColorable icon fC lC = icon # fc fC # lc lC

mapScd :: (t -> t2) -> [(t1, t)] -> [(t1, t2)]
mapScd f = map f'
        where f' (x,y) = (x, f y)

colorableList :: (Renderable (Path R2) b, Backend b R2) =>[([Char], Colour Double -> Colour Double -> Diagram b R2)]
colorableList = [
            ("mail", enveloppe ),
            ("rss", rssIcon),
            ("userGroup", userGroup),
            ("user", user),
            ("haskell", haskell),
            ("switch_off", switchOff),
            ("reload", reload),
            ("leave", leave),
            ("reload_tree", reloadTree),
            ("tree", treeIcon)
              --  ("gradExample", gradExample)
                ]

totalIconList :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) =>[([Char], Colour Double -> Colour Double -> Diagram b R2)]
totalIconList = mapScd makeColorable allIcons ++ colorableList

iconNames :: [String]
iconNames = ["overview"] ++ map fst (totalIconList :: [([Char], Colour Double -> Colour Double -> D R2)])

 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
backgroundShape' :: (PathLike b, Transformable b, HasStyle b, V b ~ R2) =>
     Colour Double -> Colour Double -> b
backgroundShape' color _ = circle 1  # fc color # lw 0

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

chunksOff ::  Int -> [a] -> [[a]]
chunksOff _ [] = []
chunksOff n notnull = (take n notnull): chunksOff n (drop n notnull)

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
                overviewImage = vcat' with { sep = 1}  $ map (vcat . (map hcat) . (chunksOff 10) . map snd . padList) [noBGnoSh, noBG, noShadow, onBackground]


