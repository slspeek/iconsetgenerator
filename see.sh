set -e
#set -x
#./clean.sh
FC=${1:-"FFA500"}
BC=${2:-"008080"}
LC=${3:-"C0C0C0"}


function callMain () {
	WIDTH=$1
	HEIGHT=$2
	FILL=$3
	BG=$4
	LINE=$5
	ICON=$6
    BACKGROUND=${7:-True}
    SHADOW=${8:-True}
    dist/build/iconset/iconset --select=$ICON\
                               --output icons/$ICON.png\
                               -w $WIDTH\
                               -h $HEIGHT\
                               --maincolor $FC\
                               --linecolor $LC\
                               --bgcolor $BG\
                               --shadow $SHADOW\
                               --background $BACKGROUND

}

ICONS_EXECUTABLE=dist/build/iconset/iconset

mkdir -p icons
PATH=$HOME/tools/bin:$PATH
#cabal clean
cabal configure
cabal build
for ICON in left_arrow\
            tree\
            reload_tree\
            reload\
            leave\
            turtle\
            user\
            info\
            userGroup\
            pencil\
            rss\
            key\
            right_arrow\
            up_arrow\
            down_arrow\
            right_triangle\
            stop\
    	    fast_forward\
            rewind\
            next\
            help\
            previous\
            hare\
            home\
            end\
            step_up\
            step_down\
            plus\
            favorite\
            zoom_in\
            zoom_out\
            heart\
            running\
            gear\
            pause\
            switch_off\
            minus
do
 $ICONS_EXECUTABLE --width 256 --height 256 --maincolor $FC --bgcolor $BC --linecolor $FC --icon $ICON --shadow --onbackground --output icons/$ICON.png
done
$ICONS_EXECUTABLE --width $((256 * 10)) --height $(( 4 * 3 * 256))  --maincolor $FC --bgcolor $BC --linecolor $LC --icon overview --output icons/overview.png --shadow --onbackground
$ICONS_EXECUTABLE --width 256 --height 256 --maincolor $FC --bgcolor $BC --linecolor $LC --icon leave --onbackground --output icons/leave.png
eog icons/turtle.png
