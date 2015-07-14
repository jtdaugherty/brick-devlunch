#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE

if [ ! -d brick ]
then
    git clone https://github.com/jtdaugherty/brick.git
fi

cabal sandbox init
cabal sandbox add-source ./brick
cabal install --only-dep -j
