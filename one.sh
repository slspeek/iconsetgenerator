set -e
#set -x
#./clean.sh
FC=${1:-"FFA500"}
BC=${2:-"008080"}
LC=${3:-"C0C0C0"}

ICONS_EXECUTABLE="runhaskell IconsetGenerator/Main.hs"

mkdir -p icons
PATH=$HOME/tools/bin:$PATH
current=unlocked
cd src
$ICONS_EXECUTABLE --width 256 --height 256 --maincolor $FC --bgcolor $BC --linecolor $LC --icon $current --onbackground --output ../icons/$current.png
cd ..
eog icons/$current.png
