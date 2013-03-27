 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.SVG.CmdLine
 --import Diagrams.Backend.Cairo.CmdLine
 
 themeFc = aqua
 
 rightTriangle size color = eqTriangle 1 # rotateBy (3/4)
                                         # fc color
                                         # lc color
                                         
 arrow size color =  rect size (size/3) # fc color 
                                        # lc color  
                                     ||| 
                     rightTriangle (size * (2/3)) color 
 
 rightArrow = arrow 1 themeFc
 leftArrow = arrow 1 themeFc # reflectX
 upArrow = rightArrow # rotateBy (1/4)
 downArrow = upArrow # rotateBy (1/2) 
 play = rightTriangle 1 themeFc
 
 
 allIcons = [ ( "right_arrow", rightArrow),
              ( "left_arrow", leftArrow),
              ( "up_arrow", upArrow),
              ( "down_arrow", downArrow),
              ("right_triangle", play) ]
 
  
 
 allIconsArg = map (padIcon 1.1) allIcons
        where padIcon  x (name, icon)  = (name, icon # pad x) 
 
 main = multiMain allIconsArg