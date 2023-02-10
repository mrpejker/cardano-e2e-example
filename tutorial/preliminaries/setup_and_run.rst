Setup and Run the example
=========================

Now that we have in mind the overall dApp architecture, we can do a
walkthrough of the necessary steps to turn on every service needed to
get the dApp running. From a top view, we need to setup and run
the server and client sides. In the server side, we will
compile and run the PAB and Budget service. For the Indexer service
we will use a public instance of Blockfrost, so we just need to get an
API token and do some minor configuration. The client side will be in
charge of running the web page engine.

We must start by cloning three repositories: the joinplank patched plutus-apps,
the budget service and the complete dApp implementation:

* ``git clone git@github.com:joinplank/plutus-apps.git``
* ``git clone git@github.com:joinplank/plutus-budget-service.git``
* ``git clone git@github.com:joinplank/cardano-e2e-example.git``

In summary, we must have the following folder structure:

.. code-block:: bash

   .
   ├── cardano-e2e-example
   │   ├── backend
   │   └── frontend
   ├── plutus-apps
   └── plutus-budget-service

The easiest way to have everything running doing a minimal configuration is
by using docker. We have docker files for the backend, frontend, and budget-service.
The instructions for running this last service can be found on the `plutus-budget-service/README <https://github.com/joinplank/plutus-budget-service/blob/main/README.md#running-the-server-through-docker>`_.
Next we will see how to use a public deployed instance of this service.

Using ``docker-compose`` we build and run the other two services. We just need
first to configure which instances of the indexer and the budget-service we will
use. Everything about the indexer can be found in the coming :ref:`subsection <indexer_blockfrost>`.
We setup the budget-service by setting the environment variable ``REACT_APP_BUDGET_URL``
on the ``cardano-e2e-example/frontend/.env``, with ``http://escrow.joinplank.com:3001/``.
After that, we just execute:

.. code-block:: bash

   $> docker-compose up --build

The ``docker-compose`` building will take several minutes. Once everthing is
running we can access de dApp on the url `http://localhost:3000 <http://localhost:3000>`_.
In case we don't want to use docker, we can install everything locally, as we
explain next.

Server side
-----------

We will use Nix for compiling and running the services.
The flow will be as follows:
enter the Nix environment, navigate to the corresponding folder, compile and run.

The first step is to install Nix from `here <https://github.com/NixOS/nix#installation>`_.
Second, we must follow `this steps <https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches>`__
to set up the binary caches provided by IOHK, so we can speed up a lot the
building process.
Generally, we just need to edit ``/etc/nix/nix.conf`` by adding the following lines:

.. code-block:: bash

   substituters = https://cache.iog.io https://cache.nixos.org/
   trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
   extra-experimental-features = nix-command flakes

Now, we must get inside the ``plutus-apps`` repository folder, and checkout the release
corresponding to `this particular commit <https://github.com/joinplank/plutus-apps/commit/a58d3a1934a4a3788da7815540a892dfe417b3bb>`_:

.. code-block:: bash

   $> git checkout a58d3a1934a4a3788da7815540a892dfe417b3bb

Then, we must run

.. code-block:: bash

   $> nix-shell

to install all the needed dependencies and get inside the development environment.
From this environment we will compile and run the services.
The first time is going to take some minutes. If the process succeeds,
you will get a new prompt indicating that you are effectively inside a
Nix environment. From now on we will assume we are always inside this environment.

.. _indexer_blockfrost:

Indexer/Blockfrost
~~~~~~~~~~~~~~~~~~

We will use a public instance of Blockfrost, so we must get a token key from its
`website <https://blockfrost.dev/docs/overview/getting-started>`_. Once
we have this token, inside the ``backend`` folder we have the ``config`` folder
where we can find the ``blockfrost-token-preprod`` file, we will put the token there.
In this particular example, the token corresponds to the preprod testnet:

.. code-block:: bash

   $> cat config/blockfrost-token-preprod
   preprod8kzHTV4w3E4WgpIZ9tpqY0YvuPwCAuht

This file will help us complete the configuration of the PAB.

The advantage of using Blockfrost is that we don't have to setup
and sync our own indexer, but if Blockfrost is not an option,
we can setup and run the plutus-apps indexer.

Budget
~~~~~~

This service allows us to evaluate Plutus scripts to know the memory and CPU
execution units. First, inside a Nix environment, we must get into the ``plutus-budget-service`` folder and run:

.. code-block:: bash

   $> cabal build budget-service

Then we can turn on the service with:

.. code-block:: bash

   $> cabal run budget-server -- --config configurations/preprod.json

Here we are using the preprod configuration of the service placed in ``configurations/preprod.json``.
If everything goes well, we should get:

.. code-block:: bash

   $> cabal run budget-server -- --config configurations/preprod.json
   Starting budget-service at port: 3001
   Quit the service with CONTROL-C.

We can check that everything is working using ``curl`` to call the ``evaluate``
endpoint of the service with the example we can find in the root folder:

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

PAB
~~~

The PAB will run the off-chain code for building unbalanced transactions, using
the indexer for querying the blockchain. First, we need to get
into de `backend` folder to compile everything with :code:`cabal build escrow-pab`.
This will take some minutes the first time.

To run this service, we will use the ``pab-config-preprod.yaml``
configuration file present in the ``backend/config`` folder, so we must be sure everything
is correctly setup there. This file has a lot of
settings, but the relevant ones for us are the Blockfrost configuration, the
wallet mode, the database, and the general PAB service.

For the Blockfrost configuration, we need to be sure we have correctly
setup the path to the file we created before with the API token.

.. code-block:: bash

   blockfrostConfig:
     bfTokenPath: ./blockfrost-token-preprod

Because we are using the PAB just for building unbalanced transactions we need
to use the remote wallet approach:

.. code-block:: bash

   walletServerConfig:
     tag: RemoteWalletConfig

For the internal PAB database the quickest setting is to use SQLite:

.. code-block:: bash

   sqliteDB:
     dbConfigFile: "pab.db"
     dbConfigPoolSize: 20

We also have the option of connecting to a PostgreSQL database.

Finally, for the PAB service general configuration, the important settings for us
are the ``baseUrl`` where we can change the hosting options, and ``permissiveCorsPolicy``
that must be set to ``True``:

.. code-block:: bash

   pabWebserverConfig:
     baseUrl: http://localhost:9080
     staticDir: plutus-pab-client/dist
     permissiveCorsPolicy: True
     endpointTimeout: 5
     enableMarconi: False

Once everything is compiled we must create the database:

.. code-block:: bash

   $> cabal run pab -- --config config/pab-config-preprod.yaml migrate

and start the PAB:

.. code-block:: bash

   $> cabal run pab -- --config config/pab-config-preprod.yaml webserver
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"tag":"RestoringPABState"},"tag":"SMultiAgent"},"tag":"PABMsg"}
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"contents":0,"tag":"PABStateRestored"},"tag":"SMultiAgent"},"tag":"PABMsg"}
   [pab:Info:15] [2023-01-01 00:00:00 UTC] {"contents":{"contents":{"contents":9080,"tag":"StartingPABBackendServer"},"tag":"SMultiAgent"},"tag":"PABMsg"}

We can use ``curl`` again to check that everything is working by hitting the ``fullreport``
endpoint of the PAB:

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

The client side, as we mentioned, is going to run the dApp webpage service. In
contrast with the server side, we don’t need a particular environment,
but we need to ensure we are using `node 16.x` version, which is as simple as
doing :code:`nvm install 16.14.2` and then :code:`nvm use 16`.

Inside the frontend folder we run ``npm install``, and before making ``npm start``
we need to setup the ``.env`` file where we setup the conection to all the services
of the server side.

.. code-block:: bash

   REACT_APP_PAB_URL='http://localhost:9080/api'
   REACT_APP_BUDGET_URL='http://localhost:3001'
   REACT_APP_BLOCKFROST_API_KEY='preprod8kzHTV4w3E4WgpIZ9tpqY0YvuPwCAuht'
   REACT_APP_BLOCKFROST_URL="https://cardano-preprod.blockfrost.io/api/v0"
   REACT_APP_LOG_LEVEL=DEBUG

Once we run ``npm start`` a browser webpage should prompt with the home page of
the escrow dApp.
