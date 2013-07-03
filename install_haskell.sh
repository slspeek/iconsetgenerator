cd ~
mkdir -p haskell_tools
HASKELL_PLATFORM=haskell-platform-2013.2.0.0.tar.gz
wget -c http://lambda.haskell.org/platform/download/2013.2.0.0/$HASKELL_PLATFORM
tar xvzf $HASKELL_PLATFORM
GHC=ghc-7.6.3-x86_64-unknown-linux.tar.bz2
wget -c http://www.haskell.org/ghc/dist/7.6.3/$GHC
tar xvjf $GHC
cd ~/ghc-7.6.3
./configure --prefix=$HOME/haskell_tools
make install
export PATH=$HOME/haskell_tools/bin:$PATH
cd ~
cd haskell-platform-2013.2.0.0
./configure --prefix=$HOME/haskell_tools
make install
