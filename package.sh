#1/bin/bash

mkdir nitro-haskell 2> /dev/null
mkdir nitro-haskell/c-bits 2> /dev/null

cp LICENSE Setup.hs nitro-haskell/
cp -r c-bits nitro-haskell/
cp -r dist/build/System nitro-haskell/

sed 's/dist\/build\///g' nitro.cabal | sed '/extra-libraries/q' > nitro-haskell/nitro.cabal

pushd "nitro-haskell" > /dev/null
cabal configure
cabal haddock
cabal sdist
popd
