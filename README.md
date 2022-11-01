# cardano-e2e-example

A simple Cardano dApp example.

## Pre-commit scripts

This repo includes a pre-commit git hook that runs hlint and checks for trailing whitespaces on staged .hs files. Before contributing you should configure git to use the correct script by running the following command:

`git config --local core.hooksPath .githooks/`
