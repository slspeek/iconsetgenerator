set -e 
PATH=$HOME/tools/bin:$PATH
ghc --make src/Main.hs
for ICON in left_arrow\
            right_arrow\
            up_arrow\
            down_arrow 
        do
            src/Main --select=$ICON -o $ICON.svg -w 800 -h 800
        done
