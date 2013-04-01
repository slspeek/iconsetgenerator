 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.Cairo.CmdLine
 import Data.Colour.SRGB
 import Diagrams.Core.Points
 import Data.Colour (withOpacity)
 
  
 
 favorite =  stroke (star (StarSkip 2) (regPoly 5 1))
 
 crossy a =  stroke ( fromVertices plusPath) # scale (8/9)
        where  plusPath =  concat . take 4 . iterate (rotateBy (-1/4)) $ spike 
               b = 1 - a 
               spike = map p2 [(-a, a), (-a,b), (a, b), (a, a)]
 
 plusIcon = crossy (1/5) 
         
 minusIcon = rect (3/2) (1/2) # scale (3/4)  
 rightTriangle size = eqTriangle 1 # rotateBy (3/4)                                      
 
                      
 leftArrow = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                    -1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac,
                                                    1/4 :: CircleFrac,
                                                    3/8 :: CircleFrac
                                                   ]
                                           [ 1/3 , 1 , a, eqSide, eqSide, a],
                               polyOrient = OrientV } # alignX 0 # scale 1.2
          where a = 1 /6
                eqSide =  2 * sqrt ( 1/3 *a) 
 
 enveloppe themeFc themeLc =  mconcat $ map stroke pathList    # lineJoin LineJoinRound 
                                               # lc themeLc
                                               # fc themeFc
                                               # centerXY
                                               # lw 0.03
                                               # scale 1.2
        where                                            
              h = 3/4
              w = 1
              ltratio = 1/3
              lower_tip = p2 ( w/2, ltratio * h)
              upper_tip = p2 ( w/2, (1 - ltratio ) * h)
              lu_corner = p2 (0, h)
              ru_corner = p2 (w, h)
              lb_corner = p2 (0, 0)
              rb_corner = p2 (w, 0)
              envList = [lu_corner, ru_corner, rb_corner, lb_corner]
              flap = fromVertices [lu_corner, lower_tip, ru_corner]
              lowerFlap = fromVertices [lb_corner, upper_tip, rb_corner]
              env =   close (fromVertices envList)
              pathList = [ flap, lowerFlap, env]
              
  
 strutedVrule = minusIcon # rotateBy (1/4)
        
 rightArrow = leftArrow # reflectX 
 upArrow = rightArrow # rotateBy (1/4)
 downArrow = upArrow # rotateBy (1/2) 
 play = rightTriangle 1 
 stepNext = (play ||| strutedVrule) # alignX 0
 stepPrevious = stepNext # reflectX 
 stepDown = stepNext # rotateBy (3/4)
 stepUp = stepDown # reflectY
 block = square 1
 fastForward = (rightTriangle 1 # alignR ||| rightTriangle 1) # scale (3/4)
 endIcon = (play ||| stepNext) # scale (3/4) # alignX 0
 home = endIcon # reflectX
 fastBackward = fastForward # reflectX
 
 
 innerCircle =  circle 0.7
 
 ring :: Path R2
 ring = circle 1 <> innerCircle

 coloredRing themeFc = stroke ring # fc themeFc # fillRule EvenOdd # lw 0
 
 
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
              transparantColor = blue `withOpacity` 0
 
 combineWithLoop diagram themeFc themeLc = ((diagram # fc themeFc # rotateBy (3/8:: CircleFrac) # scale 0.8  <>  loop themeFc themeLc) # rotateBy (-3/8:: CircleFrac) # lc themeLc)# centerXY # scale 0.5 
              
 zoomIn  =  (combineWithLoop  plusIcon)
 
 zoomOut  = combineWithLoop minusIcon
 
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
 
 hardToColor themeFc themeLc = [( "zoom_out", zoomOut themeFc themeLc),
                                ( "mail", enveloppe themeFc themeLc),
                                ( "zoom_in", zoomIn themeFc themeLc) ] 
 
 -- backgroundShape = (star (StarSkip 2) (regPoly 8 1) ) # stroke # fc themeBc # scale 1.3
 backgroundShape' color lineC = circle 1 # fc color # lc lineC
 
 setOnBackground bg lineC = map (padIcon 1.1)
        where padIcon  x (name, icon)  = (name , (icon <> backgroundShape' bg lineC) # lw 0.01 # pad x  )
         
  
 setColorOnAll fillColor lineColor = map  f  
        where f ( x, y ) = (x, y # lc lineColor # fc fillColor)
 
 
 prepareAll fc bg lc =  ( "overview", overviewImage) : allPadded        
                
        where   fcC = sRGB24read fc
                bgC = sRGB24read bg
                lcC = sRGB24read lc 
                simpleIcons = setColorOnAll fcC lcC allIcons
                hardIcons = hardToColor fcC lcC
                allList = concat [simpleIcons, hardIcons]
                allPadded = setOnBackground bgC lcC allList
                iconList = map snd allPadded
                overviewImage = hcat iconList
 
 main = do
       fcString <- getLine
       bgString <- getLine
       lcString <- getLine
       print fcString 
       print lcString
       multiMain  (prepareAll fcString bgString lcString) 