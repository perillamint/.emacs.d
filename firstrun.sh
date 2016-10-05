#!/bin/bash

git submodule init
git submodule sync

cd PG
make $@
cd ..
