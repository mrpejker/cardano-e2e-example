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

The following is a brief guide for getting everything running. It's recommended
for developers with some experience with Cardano. For a much more detailed explanation,
the `tutorial/preliminaries/setup_and_run` documentation is advised.

## Environment installation

### Haskell environment
We use `Nix` for building and running the server services, this means that the
flow to compile and run those will be as follows: enter in the (Nix) environment,
navigate to the corresponding folder, compile and run.

The first step is to install it: https://github.com/NixOS/nix#installation . Once
we have this installed:

1. We need to setup the binary catches, official guide [here](https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches)
2. Clone the joinplank patched [repository](https://github.com/joinplank/plutus-apps/)
3. Inside the `plutus-apps` folder we need to checkout a particular release
   `git checkout a58d3a1934a4a3788da7815540a892dfe417b3bb`
4. Run `nix-shell` to install all the needed dependencies and to get inside the
   development environment.

### React environment

The client side will run the dApp webpage service. In contrast with the server side,
we don’t need a particular environment, but we need to ensure we are using node
16.x version, which is as simple:
```shell
$> nvm install 16.14.2
$> nvm use 16
```

## dApp compilation and running

Running the complete dApp involves compiling and turning on a couple of services. We
need to compile and run the `PAB`, the frontend `Node Server`, and the `Budget Service`.

### PAB

We start the `nix-shell` environment from the `plutus-apps` folder, then move to the
example to get inside the `backend` folder and run `cabal build pab`. This will
take some minutes the first time. We will use the preprod testnet, so we need to
use the `blockfrost-token` for preprod and the `pab-config-preprod` file.

Before running the service we need to get a [blockfrost token](https://blockfrost.dev/docs/overview/getting-started)
and put it on the `blockfrost-token-preprod` file. Once everything is compiled
we need to create the database

`cabal run pab -- --config config/pab-config-preprod.yaml migrate`

and start the PAB

`cabal run pab -- --config config/pab-config-preprod.yaml webserver`

### Frontend

Inside the frontend folder we run ``npm install``, and before making ``npm start``
we need to setup the ``.env`` file where we setup the conection to all the services
of the server side.

```shell
REACT_APP_PAB_URL='http://localhost:9080/api'
REACT_APP_BUDGET_URL='http://localhost:3001'
REACT_APP_BLOCKFROST_API_KEY='preprod8kzHTV4w3E4WgpIZ9tpqY0YvuPwCAuht'
REACT_APP_BLOCKFROST_URL="https://cardano-preprod.blockfrost.io/api/v0"
REACT_APP_LOG_LEVEL=DEBUG
```

Once we run ``npm start`` a browser webpage should prompt with the home page of
the escrow dApp.

### Budget Service

The budget service isn't part of this repository, so we need to clone it from
[here](https://github.com/joinplank/plutus-budget-service) and follow the [README](https://github.com/joinplank/plutus-budget-service/blob/main/README.md)
steps for starting it.

## Tests

The server side implements some tests using the Emulator trace and a ContractModel
instance. The tests can be run with `cabal run tests` inside the `backend` folder.

## Pre-commit scripts

This repo includes a pre-commit git hook that runs `hlint` and checks for trailing whitespaces on staged .hs files. Before contributing you should configure git to use the correct script by running the following command:

`git config --local core.hooksPath .githooks/`
