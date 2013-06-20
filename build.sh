cabal install --reinstall --force-reinstalls 2> /dev/null
cp dist/build/System/Nitro.chs.h dist/build/System/Nitro.chs.c
cabal install --reinstall --force-reinstalls
