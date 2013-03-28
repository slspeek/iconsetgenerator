set -e 
PATH=$HOME/tools/bin:$PATH
ghc --make src/Main.hs
for ICON in left_arrow\
            right_arrow\
            up_arrow\
            down_arrow\
            right_triangle\
	    stop\
	    end\
	    fast_forward\
	    fast_backward		 
        do
            src/Main --select=$ICON -o $ICON.svg -w 800 -h 800
        done
eog *.svg
