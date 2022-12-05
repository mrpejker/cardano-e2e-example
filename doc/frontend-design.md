# Simple Escrow dApp Frontend Design

In this document we share a quick mock-up for the UI/UX of the Exchange Escrow. In this first version there is a main screen where the user can execute most actions. It also includes two pop-up screens for the Start and Cancel operations.


References:

- The **rounded** rectangles are buttons.
- The **dotted** rectangles are text fields where the user can input information.
- The **Blue** colored buttons open a new window.
- The **Green** colored buttons trigger OffChain functions in the backend.


## Main Screen

![main screen](img/mainScreen.png)

This main screen allows the user to link their wallet and connect to the contract. Then they can either start a new escrow, and cancel or resolve an existing escrow.

Once a user connects their wallet to the contract, a list of escrows they can resolve will populate automatically. Each will have a resolve button that calls the Resolve operation in the Offchain.

To start or cancel, a pop-up screen will appear, described below.

## Start

![start screen](img/startScreen.png)

When the button **New Escrow** in the main screen is triggered, this window is opened. The user complete the white spaces with the start parameters and clicks the Start button. This button calls the endpoint start from the OffChain code in the backend.

## Cancel

![cancel screen](img/cancelScreen.png)

When the **Cancel Escrow** button in the main screen is triggered, this window is opened. When the user complete the white space with the receiver address and clicks the search button, the table is completed with all the information about escrows that the user has with the receiver address. Then the user clicks the **Cancel** button that triggers the cancel endpoint on the OffChain code.