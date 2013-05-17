set -e
./grunt.sh
cabal clean
cabal configure
cabal install
(cd src/webapp/dist && iconserver)
