 {-# LANGUAGE NoMonomorphismRestriction #-}
 
 import Diagrams.Prelude
 import Diagrams.Backend.SVG.CmdLine
 --import Diagrams.Backend.Cairo.CmdLine
 
 arrow size color =  (rect size (size/3)) # fc color 
                                          # lc color  
                                     ||| 
                       eqTriangle (size * (2/3)) # rotateBy (3/4)
                                                 # fc color
                                                 # lc color 
                                                 
 
 
 
 theme_fc = aqua
 
 rightArrow = arrow 1 theme_fc
 leftArrow = arrow 1 theme_fc # reflectX
 upArrow = rightArrow # rotateBy (1/4)
 downArrow = upArrow # rotateBy (1/2) 
 
 allIcons = [ ( "right_arrow", rightArrow),
              ( "left_arrow", leftArrow),
              ( "up_arrow", upArrow),
              ( "down_arrow", downArrow) ]
 
 padIcon  x (name, icon)  = (name, icon # pad x)
  
 allIconsArg = map (padIcon 1.1) allIcons
 
 main = multiMain allIconsArg