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
            stepDown\
            mail\
            plus\
            favorite\
            minus        
        do
            src/Main --select=$ICON -o icons/$ICON.png -w 800 -h 800
        done
src/Main --select=overview -o icons/overview.png -w $(( 18 * 48 ))  -h 48
eog icons/overview.png
