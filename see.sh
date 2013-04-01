set -e 
#set -x
FC=${1:-"#7F525D"}
BC=${2:-"#737CA1"}
LC=${3:-"#C9C299"}

function callMain () {
	WIDTH=$1
	HEIGHT=$2
	FILL=$3
	BG=$4
	LINE=$5
	ICON=$6
	echo -e "$FILL\n$BG\n$LINE"|src/Main --select=$ICON -o icons/$ICON.png -w $WIDTH -h $HEIGHT 
}

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
	        rewind\
            next\
            previous\
            home\
            end\
            stepUp\
            stepDown\
            mail\
            plus\
            favorite\
            zoom_in\
            zoom_out\
            minus        
do
 callMain 100 100 $FC $BC $LC $ICON
done
 callMain $((48 * 20)) 48  $FC $BC $LC overview
eog icons/zoom_in.png
