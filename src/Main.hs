 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.Cairo.CmdLine
 import Data.Colour.SRGB
 import Diagrams.Core.Points
 
 --themeFc =  sRGB24read "#F88017"
 themeFc =  white
 themeBc =  sRGB24read "#585858"
 themeLc =  sRGB24read "#585858"
 
 
 favorite =  stroke (star (StarSkip 2) (regPoly 5 1))
 
 plusIcon = stroke ( fromVertices plusPath) # scale (8/9)
        where  plusPath =  concat . take 4 . iterate (rotateBy (-1/4)) $ spike 
               spike = map p2 [(-1/4, 1/4), (-1/4,3/4), (1/4, 3/4), (1/4, 1/4)]
         
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
 
 enveloppe =  mconcat $ map stroke pathList    # lineJoin LineJoinRound 
                                               # lc themeLc
                                               # fc themeFc
                                               # centerXY
                                               # lw 0.03
                                               # scale 1.2
        where                                            
              h = 3/4
              w = 1
              ltratio = 1/3
              lower_tip = p2 ( (w/2), ltratio * h)
              upper_tip = p2 ( (w/2), (1 - ltratio ) * h)
              lu_corner = p2 (0, h)
              ru_corner = p2 (w, h)
              lb_corner = p2 (0, 0)
              rb_corner = p2 (w, 0)
              envList = [lu_corner, ru_corner, rb_corner, lb_corner]
              flap = fromVertices [lu_corner, lower_tip, ru_corner]
              lowerFlap = fromVertices [lb_corner, upper_tip, rb_corner]
              env =   close (fromVertices envList)
              pathList = [ flap, lowerFlap, env]
              
  
 strutedVrule = strutX 0.1 ||| vrule 1 # lw 0.2 ||| strutX 0.1        
        
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
              ( "mail", enveloppe),
              ( "plus", plusIcon),
              ( "minus", minusIcon),
              ( "favorite", favorite),
              ( "fast_forward", fastForward),
              ( "fast_backward", fastBackward) ]
 
 backgroundShape = regPoly 6 1 # fc themeBc # scale 1.3
 backgroundShape' = circle 1 # fc themeBc
 
 allIconsArg = map (padIcon 1.1) allIcons
        where padIcon  x (name, icon)  = (name, (icon # fc themeFc # lc themeFc 
                                         <>  backgroundShape') # pad x  ) 
 iconList = map snd allIconsArg
 overviewImage = hcat iconList
 
  
 allIconsFinal = ( "overview", overviewImage) : allIconsArg
 
 main = multiMain allIconsFinal