set -e 
#set -x
FC=${1:-"#FFF8C6"}
BC=${2:-"#737CA1"}
LC=${3:-"#C9C299"}


function callMain () {
	WIDTH=$1
	HEIGHT=$2
	FILL=$3
	BG=$4
	LINE=$5
	ICON=$6
	echo -e "$FILL\n$BG\n$LINE"|dist/build/Iconsetgenerator/Iconsetgenerator --select=$ICON -o icons/$ICON.png -w $WIDTH -h $HEIGHT 
}

mkdir -p icons
PATH=$HOME/tools/bin:$PATH
cabal clean
cabal configure
cabal build
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
            plus\
            favorite\
            zoom_in\
            zoom_out\
            minus        
do
 callMain 72 72 $FC $BC $FC $ICON
done
callMain $((72 * 20)) 72  $FC $BC $LC overview
callMain 72 72 $FC $BC $LC mail
eog icons/favorite.png
