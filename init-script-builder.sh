#!/bin/bash

cabal new-update
cabal new-install --only-dependencies
cabal install hspec-discover
cabal new-configure --enable-tests
cabal new-test
