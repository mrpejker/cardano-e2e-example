The Balancer
============

TO BE COMPLETED

Overview
--------

First balancing: build a transaction that can be used to evaluate the scripts and calculate the budget.




First balancing
---------------

For the transaction fee, select an upper bound value.

Calculate how much has to be paid.

If something has to be paid, find UTxOs in the wallet to cover the required payment (coin selection).

Add these UTxOs as new inputs. Readjust the indexes in the transaction spending redeemers.

Add a new output for the change, paid to the CIP30 wallet change address.
If the option is set, merge with other outputs directed to the same address.

Set collateral. Set other low-level information (e.g. the script data hash).


