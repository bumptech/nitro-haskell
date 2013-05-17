Haskell bindings for Nitro

Requirements: redo, libev, gcc-4.7, ghc >= 7.6.1

1. build nitro: nitro/README.md

2. symlink gcc to gcc-4.7 (and unlink it if not desired default)

       sudo cp /usr/bin/gcc /usr/bin/gcc-default

       sudo ln -s /usr/bin/gcc-4.7 /usr/bin/gcc

   This step is necessary because cabal cannot currently choose a gcc version

3. compile haskell bindings

   ./build.sh
