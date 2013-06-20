#!/bin/bash
GCC=gcc-4.7

pushd "nitro" >/dev/null
echo "installing nitronacl, this may take a few minutes"
redo nacl
echo "installing nitro"
CC=$GCC redo
CC=$GCC redo install
popd

mkdir c-bits 2> /dev/null
cp nitro/nacl-*/build/$HOSTNAME/include/*/*.h c-bits/
cp -r nitro/src/* c-bits/
echo "successful configuration"
