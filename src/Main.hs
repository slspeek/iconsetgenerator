 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.SVG.CmdLine
 import Data.Colour.SRGB
 
 themeFc =  sRGB24read "#FFFFFF"
 themeBc =  sRGB24read "#000000"
 themeLc =  blue
 
 
 rightTriangle size = eqTriangle 1 # rotateBy (3/4)                                      
 
                      
 leftArrow = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                  -1/4 :: CircleFrac,
                                                  3/8 :: CircleFrac,
                                                  1/4 :: CircleFrac,
                                                  3/8 :: CircleFrac
                                                  ]
                                                [ 1/3 ,
                                                 1 ,
                                                  a,
                                                   eqSide,
                                                    eqSide,
                                                     a],
                               polyOrient = OrientV } # alignX 0
          where a = 1 /6
                eqSide =  2 * sqrt ( 1/3 *a) 
 
 enveloppe =  mconcat $ map stroke pathList    # lineJoin LineJoinRound 
                                               # lc themeLc
                                               # fc themeFc
                                               # centerXY
                                               # lw 0.03
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
              ( "right_triangle", play),
              ( "stop", block),
              ( "home", home),
              ( "stepUp", stepUp),
              ( "stepDown", stepDown),
              ( "end", endIcon),
              ( "mail", enveloppe),
              ( "fast_forward", fastForward),
              ( "fast_backward", fastBackward) ]

 allIconsArg = map (padIcon 1.1) allIcons
        where padIcon  x (name, icon)  = (name, (icon # fc themeFc
                                                     # lc themeFc 
                                                     <> circle 1 # fc themeBc) # pad x  ) 
 
 main = multiMain allIconsArg