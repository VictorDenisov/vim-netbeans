#!/bin/bash -e

cabal build
./dist/build/Main/Main > log.txt &
vim -nb:localhost:4444:password
