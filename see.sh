set -e 
mkdir -p icons
PATH=$HOME/tools/bin:$PATH
ghc --make src/Main.hs
for ICON in left_arrow\
            right_arrow\
            up_arrow\
            down_arrow\
            right_triangle\
	        stop\
    	    fast_forward\
	        fast_backward\
            next\
            previous\
            home\
            end\
            stepUp\
            stepDown         
        do
            src/Main --select=$ICON -o icons/$ICON.svg -w 800 -h 800
        done
eog icons/*.svg
