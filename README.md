# composite-realworld

- [Resources](#resources)
- [Generate & Source the RSA Keyfile](#generate--source-the-rsa-keyfile)

## Warnings
*ACHTUNG!*: per the RealWorld specification, this application uses JWTs for
user authentication. My usage hasn't been audited, and I'm not especially
knowledgable about web security, so take this with a grain of salt!

## Resources
A lot of this code has been copied, adapted, or at the very least least inspired
by some of the following examples/libraries:

- [Matt Parsons]' [servant-persistent]
- [Confer Health]'s [composite] 
- [Chris Allen] and [Alexey Zabelin]'s [cards-with-comrades]

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

[Confer Health]: https://github.com/ConferHealth
[composite]: https://github.com/ConferHealth/composite
[Matt Parsons]: https://github.com/parsonsmatt
[servant-persistent]: https://github.com/parsonsmatt/servant-persistent
[Chris Allen]: https://github.com/bitemyapp
[Alexey Zabelin]: https://github.com/alexeyzab
[cards-with-comrades]: https://github.com/alexeyzab/cards-with-comrades
