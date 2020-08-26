# Chainweaver Changelog

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

