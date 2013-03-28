 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.SVG.CmdLine
 import Data.Colour.SRGB
 
 themeFc =  sRGB24read "#FFFFFF"
 themeBc =  sRGB24read "#000000"
 
 rightTriangle size = eqTriangle 1 # rotateBy (3/4)                                      
 
                      
 leftArrow' size = polygon with { polyType   = PolySides [ 1/4 :: CircleFrac,
                                                  -1/4 :: CircleFrac,
                                                  3/8 :: CircleFrac,
                                                  1/4 :: CircleFrac,
                                                  3/8 :: CircleFrac
                                                  ]
                                                [ size/3 ,
                                                 size ,
                                                  a,
                                                   eqSide,
                                                    eqSide,
                                                     a],
                               polyOrient = OrientV } # alignX 0
          where a = size /6
                eqSide =  2 * sqrt ( size/3 *a) 
 
 
 
 strutedVrule = strutX 0.1 ||| vrule 1 # lw 0.2 ||| strutX 0.1        
        
 leftArrow = leftArrow' 1
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
              ( "fast_forward", fastForward),
              ( "fast_backward", fastBackward) ]

 allIconsArg = map (padIcon 1.1) allIcons
        where padIcon  x (name, icon)  = (name, (icon # fc themeFc
                                                     # lc themeFc 
                                                     <> circle 1 # fc themeBc) # pad x  ) 
 
 main = multiMain allIconsArg