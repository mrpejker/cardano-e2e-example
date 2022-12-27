Setup and Run the example
=========================

Now we have in mind the overall dApp architecture, we can do a
walkthrough of the necessary steps to turn on every service needed to
get the dApp running. From a top view, we need to get setup and run
the server and client side. On the server side, we are going to
compile and run the PAB and Budget service, and given we are going to
use a public instance of Blockfrost for the indexer, we just need to
get a api-token and do some minor configuration. The client side will be in
charge of running the web page engine.

We must start by cloning three repositories, the joinplank patched plutus-apps
`repository <https://github.com/joinplank/plutus-apps/>`_, the complete dApp
implementation that can be found `here <https://github.com/joinplank/cardano-e2e-example/>`__ and
the budget service from `here <https://github.com/joinplank/plutus-budget-service/>`__.
In summary, we must have the following folder structure:

.. code-block:: bash

   .
   ├── cardano-e2e-example
   │   ├── contract
   │   └── frontend
   ├── plutus-apps
   └── plutus-budget-service

Server side
-----------

We will use the nix approach for compiling and running the server services, this
means that the flow to compile and run those will be as follows:
enter in the (nix) environment, navigate to the corresponding folder, compile and run.

The first step is to install nix from `here <https://github.com/NixOS/nix#installation>`_.
Once we have this installed, we will set up the binary catches dependencies from the
official IOG sources that help speed up the complete build of the
``plutus-pab`` library. The steps for this are simple and can be found
on the official repository `here <https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches>`__.
In general, we use the non-NixOS option, so we just need to edit
the ``/etc/nix/nix.conf`` file by adding the following lines:

.. code-block:: bash

   substituters = https://cache.iog.io https://cache.nixos.org/
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   extra-experimental-features = nix-command flakes

With everything setup we need to get inside the ``plutus-apps`` folder, and
checkout a particular release:

.. code-block:: bash

   $> git checkout a58d3a1934a4a3788da7815540a892dfe417b3bb

So then we can execute :code:`nix-shell` to install all the needed
dependencies and get inside the development environment, that is from
where we are going to compile and run the server services. The first
time is going to take some minutes. From now on we we will assume we are always
inside this environment.

Indexer/Blockfrost
~~~~~~~~~~~~~~~~~~

We are going to use a public instance of Blockfrost, so one of the first
things we need to do is to get a token key from its
`website <https://blockfrost.dev/docs/overview/getting-started>`_. Once
we have this token, inside the ``contract`` folder we need to create a
file, let's call it ``blockfrost-token``, and write the token there. In
this particular example, the token corresponds with the preprod testnet.


.. code-block:: bash

   $> cat blockfrost-token
   preprod8kzHTV4w3E4WgpIZ9tpqY0YvuPwCAuht

This file is going to help us to complete the configuration of the PAB.

The advantage of starting with this instance of Blockfrost is that we
don’t need to sync anything, but we also can use the official
plutus-apps indexer, which in contrast will require some sync time.

Budget
~~~~~~

This service allows us to evaluate plutus scripts to know the memory and cpu
execution units. First, we must get into the ``plutus-budget-service`` folder and execute
:code:`cabal build budget-service`. Then we can turn on the service with
:code:`cabal run budget-server -- --config configurations/preprod.json`. Here
we are using the preprod configuration of the service placed in ``configurations/preprod.json``.
If everything went well, we should get:

.. code-block:: bash

   $> cabal run budget-server -- --config configurations/preprod.json
   Starting budget-service at port: 3001
   Quit the service with CONTROL-C.

we can check that everything is working using ``curl`` and hitting the ``evaluate``
endpoint of the service with the example we can find in the folder

.. code-block:: bash

   $> curl -X POST localhost:3001/evaluate -H 'Content-Type: application/json' -d @example.json | jq
   {
     "Right": {
       "Mint:0": {
         "exUnitsSteps": 422176029,
         "exUnitsMem": 1396682
       },
       "Spend:3": {
         "exUnitsSteps": 466510658,
         "exUnitsMem": 1549708
       },
       "Spend:0": {
         "exUnitsSteps": 1120532675,
         "exUnitsMem": 4164373
       }
     }
   }

Plutus Application Backend
~~~~~~~~~~~~~~~~~~~~~~~~~~

The PAB will run the off-chain code for building unbalanced transactions and also
is going to use the indexer for querying the blockchain. First, we need to get
into de `contract` folder to compile everything with :code:`cabal build escrow-pab`.
This will take some minutes the first time.

To run this service, we are going to use the ``pab-config.yaml``
configuration file that is already present on the ``contract`` folder, so we need
to be sure everything is on the correct setup there. This file has a lot of
settings, but the relevant ones for us are the blockfrost configuration, the
wallet mode, the database, and the general PAB service.

For the blockfrost configuration, we need to be sure we have correctly
setup the path to the file we created before with the API token.

.. code-block:: bash

   blockfrostConfig:
     bfTokenPath: ./blockfrost-token

Because we are using the PAB just for building unbalanced transactions we need
to use the remote wallet approach.

.. code-block:: bash

   walletServerConfig:
     tag: RemoteWalletConfig

The PAB has its own database, the quickest setting is to use SQLite but we also
have the possibility to setup a PostgreSQL service and use that.

.. code-block:: bash

   sqliteDB:
     dbConfigFile: "pab.db"
     dbConfigPoolSize: 20

Finally, the PAB service general configuration, here the important settings for us
are the ``baseUrl`` where we can change the hosting options, and ``permissiveCorsPolicy``
that must be setup on ``True``.

.. code-block:: bash

   pabWebserverConfig:
     baseUrl: http://localhost:9080
     staticDir: plutus-pab-client/dist
     permissiveCorsPolicy: True
     endpointTimeout: 5
     enableMarconi: False

Once everything is compiled we need to create the database

.. code-block:: bash

   $> cabal run pab -- --config pab-config.yaml migrate

and start the PAB

.. code-block:: bash

   $> cabal run pab -- --config pab-config.yaml webserver
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"tag":"RestoringPABState"},"tag":"SMultiAgent"},"tag":"PABMsg"}
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"contents":0,"tag":"PABStateRestored"},"tag":"SMultiAgent"},"tag":"PABMsg"}
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"contents":9080,"tag":"StartingPABBackendServer"},"tag":"SMultiAgent"},"tag":"PABMsg"}

we can use ``curl`` again to check that everything is working by hitting the ``fullreport``
endpoint of the PAB.

.. code-block:: bash

   $> curl -X GET localhost:9080/fullreport -H 'Content-Type: application/json' | jq
   {
     "chainReport": {
     "annotatedBlockchain": [],
     "transactionMap": [],
     "utxoIndex": {
       "getIndex": []
     }
   },
    "contractReport": {
      "crActiveContractStates": [],
      "crAvailableContracts": []
    }
   }

Also, we can access the swagger through `localhost:9080/swagger/swagger-ui <http://localhost:9080/swagger/swagger-ui>`_.

Client side
-----------

The client side, as we mentioned, is going to run the web server. In
contrast with the server side, we don’t need a particular environment,
but we need to ensure we are using `node 16.x` version, which is as simple as
doing :code:`nvm install 16.14.2` and then :code:`nvm use 16`.

Inside the frontend folder we run ``npm install``, and before making ``npm start``
we need to setup the ``.env`` file where we setup the conection to all the services
of the server side.

.. code-block:: bash

   # For development use http://localhost:3000/api (has a proxy to 9080)
   REACT_APP_PAB_URL='http://localhost:3000/api'
   REACT_APP_ESTIMATOR_URL='http://localhost:3001'
   REACT_APP_BLOCKFROST_API_KEY='preprod8kzHTV4w3E4WgpIZ9tpqY0YvuPwCAuht'
   REACT_APP_BLOCKFROST_URL="https://cardano-preprod.blockfrost.io/api/v0"
   REACT_APP_LOG_LEVEL=DEBUG

Once we run ``npm start`` a browser webpage should prompt with the home page of
the escrow dApp.
