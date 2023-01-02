# A simple Cardano dApp example [![Haskell CI](https://github.com/joinplank/cardano-e2e-example/actions/workflows/haskell.yml/badge.svg?branch=main)](https://github.com/joinplank/cardano-e2e-example/actions/workflows/haskell.yml)

The main goal of this dApp implementation and tutorial is to present a design
pattern of a working end-to-end example.

## Folder structure

```shell
.
├── backend
├── frontend
├── doc
├── tutorial
└── README.md
```
The documentation can be found in the `doc` folder, where we have the different
requirements and design documents. The `tutorial` folder has the complete tutorial
explaining every design pattern, decision, etc. The `backend` and `frontend` folders
contain the Haskell server-side implementation and the React implementation of
the webpage, respectively.

## Environment installation

### Haskell environment
We use `nix` for building the complete dApp, thus the first step is to install
it: https://github.com/NixOS/nix#installation . Once we have this installed:

1. We need to setup the binary catches, official guide [here](https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches)
2. Clone the joinplank patched [repository](https://github.com/joinplank/plutus-apps/)
3. Inside the `plutus-apps` folder we need to checkout a particular release
   `git checkout a58d3a1934a4a3788da7815540a892dfe417b3bb`
4. Run `nix-shell` to install all the needed dependencies and to get inside the
   development environment.

### React environment

1. make sure you're using version 16 of npm:
```
nvm install 16.14.2
nvm use 16
```
## dApp compilation and running

Running the complete dApp involves compiling and turning on a couple of services. We
need to compile and run the `PAB`, the frontend `server`, and the `Budget Service`.

### PAB

We start the nix-shell environment from the plutus-apps folder, then move to the
example to get inside the `backend` folder and run `cabal build pab`. This will
take some minutes the first time. Before running the service we need to get a
[blockfrost token](https://blockfrost.dev/docs/overview/getting-started) and put
it on the `blockfrost-token` file. Once everything is compiled we need to create
the database

`cabal run pab -- --config pab-config-preprod.yaml migrate`

and start the PAB

`cabal run pab -- --config pab-config-preprod.yaml webserver`

### Frontend

Inside the `frontend` folder we run `npm install`, and before making `npm start`
we need to setup the `.env` file.

### Budget Service

The budget service isn't part of this repository, so we need to clone it from
[here](https://github.com/joinplank/plutus-budget-service) and follow the [README](https://github.com/joinplank/plutus-budget-service/blob/main/README.md)
steps for starting it.

## Pre-commit scripts

This repo includes a pre-commit git hook that runs `hlint` and checks for trailing whitespaces on staged .hs files. Before contributing you should configure git to use the correct script by running the following command:

`git config --local core.hooksPath .githooks/`
