#!/bin/bash
GCC=gcc-4.7

echo "unpacking nitro"
git submodule init
git submodule update

pushd "nitro" >/dev/null
echo "installing nitronacl"
redo nacl
echo "installing nitro"
CC=$GCC redo
CC=$GCC redo install
popd

mkdir c-bits 2> /dev/null
cp nitro/nacl-*/build/$HOSTNAME/include/*/*.h c-bits/
cp -r nitro/src/* c-bits/
echo "successful configuration"
