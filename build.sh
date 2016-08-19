#!/bin/bash

set -e

cabal install .
cabal exec -- trialrun
