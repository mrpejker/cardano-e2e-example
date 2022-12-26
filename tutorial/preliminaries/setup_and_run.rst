Setup and Run the example
=========================

Now we have in mind the overall dApp architecture, we can do a
walkthrough of the necessary steps to turn on every service needed to
get the dApp running. From a top view, we need to get setup and run
the server and client side, but first, we need to install all the
necessary library dependencies and environments.

Preliminaries
-------------

We will use the nix approach for the **server side**, this means that
the flow to compile and run the server services will be as follows:
enter in the (nix) environment, navigate to the corresponding folder,
compile and run.

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

With everything setup, we need to clone the joinplank patched `repository <https://github.com/joinplank/plutus-apps/>`_,
get inside the ``plutus-apps`` folder, and checkout a particular release:

.. code-block:: bash

   $> git checkout a58d3a1934a4a3788da7815540a892dfe417b3bb

So then we can execute :code:`nix-shell` to install all the needed
dependencies and get inside the development environment, that is from
where we are going to compile and run the server services. The first
time is going to take some minutes.

On the **client side**, we are not going to use any particular
environment, but we need to ensure we are using the ``node 16.x``
version, which is as simple as doing :code:`nvm install 16.14.2` and then
:code:`nvm use 16`.

Last, we need to clone the complete dApp implementation that can be
found `here <https://github.com/joinplank/cardano-e2e-example/>`__ and
the budget service from `here <https://github.com/joinplank/plutus-budget-service/>`__.
In summary, we should have the following folder structure:

.. code-block:: bash

   .
   ├── cardano-e2e-example
   │   ├── contract
   │   └── frontend
   ├── plutus-apps
   └── plutus-budget-service

Server side
-----------

As we mentioned, compiling and run every server service will require
the nix environment, so we need to first get into the environment by
executing ``nix-shell`` inside the ``plutus-apps`` folder. For this
section, we will assume we are always inside this environment. The server side is
composed of three services. The indexer, the budget evaluator, and the PAB.

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

PAB
~~~



Client side
-----------
