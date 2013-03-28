 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.SVG.CmdLine
 
 themeFc = blue
 
 rightTriangle size = eqTriangle 1 # rotateBy (3/4)                                      
                                         
 arrow size =  rect size (size/3) ||| rightTriangle (size * (2/3)) 
 
 rightArrow = arrow 1 
 leftArrow = rightArrow # reflectX
 upArrow = rightArrow # rotateBy (1/4)
 downArrow = upArrow # rotateBy (1/2) 
 play = rightTriangle 1  
 block = unitSquare # pad 1.2
 endIcon = rightArrow ||| (vrule 1 # lw 0.2)
 fastForward = rightTriangle 1 ||| rightTriangle 1
 fastBackward = fastForward # reflectX
 
 allIcons = [ ( "right_arrow", rightArrow),
              ( "left_arrow", leftArrow),
              ( "up_arrow", upArrow),
              ( "down_arrow", downArrow),
              ( "right_triangle", play),
              ( "stop", block),
              ( "end", endIcon),
              ( "fast_forward", fastForward),
              ( "fast_backward", fastBackward) ]

 allIconsArg = map (padIcon 1.1) allIcons
        where padIcon  x (name, icon)  = (name, icon # pad x
                                                     # fc themeFc
                                                     # lc themeFc   ) 
 
 main = multiMain allIconsArg