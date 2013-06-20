## Haskell bindings for Nitro
=============================

Requirements: redo, libev, gcc-4.7, ghc >= 7.6.1


### Build

```
git submodule init
git submodule update

sudo ./configure.sh

./build.sh
```

Tips for failed configuring

* Check that you have redo, libev, gcc-4.7 installed
* Your -Wall may be more restrictive than the platform on which nitro was originally built (debian unstable).  For instance on Ubuntu 12.04, -Werror=format-security -Werror=unused-result prevents compilation.  In nitro/default.o.do, explicity -Wno-error these:
```
$CC -O2 -fno-strict-aliasing -Wall -Werror -Wno-error=format-security -Wno-error=unused-result -fPIC -g -Isrc -I$NACL_INC -c $path.c -o $3
```
* Check that you configure with sudo

### Packaging

```
./package.sh
```

### Usage

View the hackage documentation [coming soon]

Run the examples:

```
./run-examples.sh
```
