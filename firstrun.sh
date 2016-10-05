#!/bin/bash

git submodule init
git submodule sync
git submodule update

cd PG
make $@
cd ..
