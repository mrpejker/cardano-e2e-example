Modules Design
==============

.. image:: ../img/modules_dependencies.png
   :width: 100%

The dependency graph exhibits the organization of the library, which
is basically implemented into two main parts, the :code:`Escrow` where
we can find the relevant implementation of the *smart contract* and
the :code:`Utils` where we have some general helper functions that are
totally agnostic of this particular *smart contract*.  In particular,
the module :code:`WalletAddress` is pretty handy as a simplified
version of the :code:`Ledger` type :code:`Address`.

The module structure of the :code:`Escrow` folder encapsulates three main
parts. The :code:`Business` module, that is agnostic of the particular details
of the blockchain and only contains the data type definitions and
functions for abstractly handling these. For instance, it could be
common on a dApp to have a kind of internal state that we need to
create, modify, and delete. So, in this module, we define the
corresponding data type representation of that state and all the
functionality needed for computing things over it. One important
aspect of this module is that it will be shared between the off-chain
and on-chain implementation avoiding any mismatch during the
modification and manipulation of the internal state. The :code:`Types` module
is related to this one, but here we have the data types definitions in
the context of the blockchain. For instance, we implement the datum
and redeemer type.

The :code:`OnChain` and :code:`Validator` modules implement the
contract script for validating the spending of the corresponding UTxO
and the compilation from the Haskell implementation to Plutus Core.

The :code:`OffChain` groups together all the functionality in charge
of building an unbalanced transaction and querying the blockchain. Here we
implement the interface and behavior of each operation that, in the end, will
define the API of the contract.
