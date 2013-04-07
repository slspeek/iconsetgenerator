set -e
#set -x
./clean.sh
FC=${1:-"#FFFF3D"}
BC=${2:-"#1B00A1"}
LC=${3:-"#C9C299"}


function callMain () {
	WIDTH=$1
	HEIGHT=$2
	FILL=$3
	BG=$4
	LINE=$5
	ICON=$6
    BACKGROUND=${7:-True}
    SHADOW=${8:-True}
	echo -e "$FILL\n$BG\n$LINE\n$SHADOW\n$BACKGROUND"|dist/build/Iconsetgenerator/Iconsetgenerator --select=$ICON -o icons/$ICON.png -w $WIDTH -h $HEIGHT 
}

mkdir -p icons
PATH=$HOME/tools/bin:$PATH
cabal clean
cabal configure
cabal build
for ICON in left_arrow\
            gradExample\
            pencil\
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
            heart\
            running\
            gear\
            minus
do
 callMain 256 256 $FC $BC $FC $ICON
done
callMain $((256 * 20)) $(( 3 * 256))  $FC $BC $LC overview
callMain 256 256 $FC $BC $LC mail
callMain 256 256 $FC $BC $LC pencilExample True False 
eog icons/gear.png
