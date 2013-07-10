## Haskell bindings for Nitro
=============================

Requirements:
* libev
* gcc-4.7
* ghc >= 7.6.1
* [nitro](https://github.com/bumptech/nitro) >= 0.2
* [sodium](https://github.com/jedisct1/libsodium/releases) >= 4.2

### Build
```
autoreconf
cabal configure
./build.sh
```

### Packaging
```
./package.sh
```

### Usage
View the [hackage documentation](haskell.gonitro.io)

### Run the examples
```
./run-examples.sh
```
