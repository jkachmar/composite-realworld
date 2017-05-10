# composite-realworld

[![Build Status](https://travis-ci.org/jkachmar/composite-realworld.png)](https://travis-ci.org/jkachmar/composite-realworld)

- [Generate & Source the RSA Keyfile](#generatesource-the-rsa-keyfile)

## Generate & Source the RSA Keyfile
In order to run this server, an RSA keyfile must be generated and the environment
variable `KEYPATH` must be set to the filepath where the keyfile is located.

As an example, from the project's root directory:

- Generate the keys: `ssh-keygen -t rsa -b 4096 -f ./resources/keys/compositeRealWorld.key`
- Source the environment variable by either...
  - Appending `KEYPATH='./resources/keys/compositeRealWorld.key'` to the end of a `.env` file
  - Sourcing it in the terminal session: `export KEYPATH='./resources/keys/compositeRealWorld.key'`

This keyfile will now be loaded into the application upon initialization, and
used to construct a JSON Web Key (JWK).
