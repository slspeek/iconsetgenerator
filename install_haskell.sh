cd ~
HASKELL_PLATFORM=haskell-platform-2012.4.0.0.tar.gz
wget http://lambda.haskell.org/platform/download/2012.4.0.0/$HASKELL_PLATFORM
tar xvzf $HASKELL_PLATFORM
GHC=ghc-7.4.2-x86_64-unknown-linux.tar.bz2
wget http://www.haskell.org/ghc/dist/7.4.2/$GHC
tar xvjf $GHC
cd ~/ghc-7.4.2
./configure --prefix=~/haskell_tools
make install
cd ~
cd haskell-platform-2014.4.0.0
./configure --prefix=~/haskell_tools
make install
