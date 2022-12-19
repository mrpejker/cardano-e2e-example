Cardano dApp Architecture
=========================

In this example we'll follow a Server-Client architecture.
On the server side, we run an http web-server which provides an API
for connecting with the dApp operations. Each operation will run the
necessary logic for reading the state on the blockchain and, in most
of the cases, building an unbalanced transaction.
On the client side, a web frontend connects to the web-server for getting
information from the blockchain and getting unbalanced transactions. Then
it balance, sign and sumbit them, completing the dApp flow.



.. figure:: /img/architecture.png
