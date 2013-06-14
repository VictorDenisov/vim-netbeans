#!/bin/bash -e

cabal build
./dist/build/framework-tests/framework-tests
