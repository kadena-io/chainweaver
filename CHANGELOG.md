# Chainweaver Changelog

## 2.2.1 (2021-12-17)
* k: account support
* Copy buttons for accounts and keys
* New default gas prices
* Warnings when using legacy accounts

## 2.2.0 (2021-09-13)

* Obelisk bump to v0.9.1.0 with reflex-platform 0.7.0.0
* Pact 4.0 support
* Default to free-x-chain-gas for cross chain transfers to accounts that don't exist or have 0 balance
* Cross-chain and transfer "listen" calls switched to repeated polling calls
* Update default network list to support new nodes
* Remove keys and accounts api endpoint
* Fixes issue where 'Done' button showed up too early on cross-chains
* Fix multiple parsing issues with 'Amount' input field in tx-widget
* Fix issue with keyset-ref display
* Allows a dapp to specify that Chainweaver's user may choose its own 'sender' for signing api
  requests
* Fix issue causing chain-based account notes to not persist for first 10 chains
* Ship with version 4.8.8 of z3
* Fix no gas payer on destination chain error message
* Number of changes in preparation for Chainweaver-Web, including the removal of monad-logger and
  fast-logger dependencies, and an update of kadena-signing-api

## 2.1 (2020-08-26)

* Fix ordering of ChainId dropdown to work with 20 chains
* Add ability to transfer an account's whole balance without leaving dust
* Make signing info available as a QR code
* Reduce default gas price
* Add safe bidirectional transfers that guarantee you are not sending coins into the abyss

## 2.0 (2020-05-13)

This version features a greatly simplified transfer interface as well as
numerous other features and smaller improvements.

### UX/UI improvements

* Improved the general ease of use on main work flows
* More meaningful and accurate in-app guides and descriptions 
* Automatic generation of an initial key & account on the first launch
* Streamlined frequently used functionalities such as accounts and coin transfers
* Add a dedicated receive button with auto generation of TX builder information
* One-click copying of Tx Builder
* Easy way to see which accounts you own solely, jointly, or not at all

### Transfer Tool

* A dedicated and convenient tool with support for all of the possible scenarios
* Supports signing with Chainweaver-generated and externally-generated keys
* Fully offline signing of transfers using cold wallets
* Full support for transfers to and from multi-sig accounts
* Cross chain transfers in all of the above scenarios
* Pasting a Tx Builder into the account section automatically fills out the owner keyset
* Detects when your account name is a public key and auto-creates an appropriate keyset

### New Features

* Settings: Change password
* Settings: Export/Import wallet (also serves as new recovery method)
* Settings: Export transaction log
* Ability to Check Tx Status with a Request Key

