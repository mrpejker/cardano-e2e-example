.. _frontend:

Escrow frontend
===============

The escrow frontend is a standard web application based on Node.js and React.
In the frontend, all the pieces are connected together to support the complete
functionality provided by the escrow dApp.

User interface
--------------

The user interface is defined `here <https://github.com/joinplank/cardano-e2e-example/blob/main/frontend/src/components/EscrowUI/index.tsx>`_
in the ``EscrowUI`` function. It is implemented following the `frontend design <https://github.com/joinplank/cardano-e2e-example/blob/main/doc/frontend-design.md>`_
document. It looks as follows:

.. figure:: /img/user_interface.png

All the UI components interact with the modules contained in the
``contractEndpoints`` folder, where the main class and necessary datatypes are
defined.


Main class
----------

Our approach is to define a single main class ``EscrowEndpoints`` that is in
charge of providing the complete end-to-end flow for every dApp operation and
also for providing the information we want to show in the user interface.

To instantiate we call:

* ``connect(walletName: "nami" | "eternl")``

At instantiation, a contract instance is activated in the PAB by using the
``ContractEndpoints`` module. Also, a connection to the budget service is
initialized, and Blockfrost is queried to obtain the protocol parameters and
create an instance of the balancer.

Other important parameters are taken from the environment using the ``getEnvs``
function:

* ``REACT_APP_PAB_URL``: PAB URL
* ``REACT_APP_BUDGET_URL``: Budget service URL
* ``REACT_APP_BLOCKFROST_URL``: Blockfrost URL
* ``REACT_APP_BLOCKFROST_API_KEY``: Blockfrost API token

Once instantiated, the available methods are:

* ``start(sp: StartParams)``
* ``cancel(cp: CancelParams)``
* ``resolve(rp: ResolveParams)``
* ``reload()``

Methods ``start``, ``cancel`` and ``resolve`` corresponds to dApp operations,
so they all follow the same implementation pattern:

1. Obtain an unbalanced transaction from the PAB by calling ``doOperation`` of
   the contract endpoints.
2. Balance the transaction by calling ``fullBalanceTx`` of the balancer.
3. Sign and submit the transaction by calling signAndSubmit of the wallet.

Last, ``reload`` method returns the list of escrow than can be resolved by
calling ``reload`` of the contract endpoints to get the observable state.


Parameters
----------

As we are using typescript, together with the main class we define dApp
specific datatypes. A first group of types is defined in ``parameters.tsx`` and
is used to build valid parameters for the endpoint calls:

* ``StartParams``
* ``CancelParams``
* ``ResolveParams``

The type definitions must match the format expected by the PAB, as defined in
the Haskell interface and described in the `endpoints specification <https://github.com/joinplank/cardano-e2e-example/blob/main/doc/endpoints-spec.md>`_
document. 

Together with the types, we define Haskell-style "smart constructors" to be able to easily build the parameters from the primitive javascript types used in the HTML forms.


Observable state
----------------

Another group of types is related to the observable state provided by the PAB and the information displayed in the UI.

The type ``PABObservableState`` represents the observable exactly as returned
by the reload method of ``ContractEndpoints``, and therefore it must match the
JSON format of the observable state field returned by the PAB's status endpoint.

For the escrow, the observable state has the Haskell type ``[UtxoEscrowInfo]``
which has a default JSON serialization by deriving the ``ToJSON`` class
instantiation.

In the frontend code, we choose to parse the PAB's observable state and return
a more UI friendly type ``ObsState``, defined as a list of objects of type
``EscrowInfo``.
