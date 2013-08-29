#!/bin/sh

# This is very rough and permanent.
# TODO: learn how to use nix-env to make a temp environment?

nix-env -i haskell-Elm-ghc7.5.3
nix-env -i elm-server

