#1/bin/bash

mkdir nitro-haskell 2> /dev/null

cp -r include nitro-haskell/
cp LICENSE Setup.hs configure nitro.buildinfo.in nitro-haskell/
cp -r dist/build/System nitro-haskell/

sed 's/dist\/build\///g' nitro.cabal | sed '/c2hs/d' | sed '/c-sources/q' > nitro-haskell/nitro.cabal

pushd "nitro-haskell" > /dev/null
cabal configure
cabal haddock --hyperlink-source
cabal sdist
popd
