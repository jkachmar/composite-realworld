# composite-realworld

## Warnings

### On Overengineering

The RealWorld project aims to provide examples that aren't overcomplicated or
overengineered so that people unfamiliar with a framework can see what an
idiomatic MVP written with it looks like.

Contrary to that statement, this repository feels pretty overengineered at first
glance. I mainly started this as a learning exercise to familiarize myself
with [servant-auth], [composite], [opaleye], [refurb], and larger Haskell 
applications in-general, but at this point it feels like something that someone
might find helpful at some point.

### On Security

*ACHTUNG!*: per the RealWorld specification, this application uses JWTs for user
authentication. I haven't had my implementations audited by anyone, and I'm not 
especially knowledgable about web security, so caveat emptor!

## Getting Started

### Prerequisites

Before building this application, ensure the following dependencies are 
installed on your system:

- [stack](https://docs.haskellstack.org/en/stable/README/)
- [PostgreSQL](https://www.postgresql.org)*

*This application was developed on PostgreSQL 9.6.3, but should be compatible
with any recent version of PostgreSQL supporting the `uuid-ossp` extension.

### TL;DR

To get started as quickly as possible:

    git clone git@github.com/jkachmar/composite-realworld
    cd composite-realworld
    stack setup

    cd migrations
    stack build
    make init
    stack exec migrate -- -c './resources/refurb/config' migrate --seed --execute

    cd ../
    ssh-keygen -t rsa -b 4096 -N "" -f ./resources/keys/compositeRealWorld.key
    stack build
    stack exec composite-realworld-exe

The rest of this README will try to explain in-depth what is happening during
the steps above.

#### Get the Project

Clone this repository and navigate to the directory

    git clone git@github.com/jkachmar/composite-realworld
    cd composite-realworld

Install the Glasgow Haskell Compiler (GHC) and related tooling

    stack setup

#### Initialize, Migrate, and Seed the Database

This application uses [refurb](https://github.com/ConferHealth/refurb) to manage
migrations and seeding of test data. In addition, a `Makefile` and some SQL 
scripts are included to help setup and teardown a testing database.

To keep things relatively simple, the migration utilities live in their own
Haskell project in the `migrations` subdirectory. Within this directory is a
`config` file that provides `refurb` with PostgreSQL connection details; 
currently this project is set up assuming a local environment with the standard
Postgres defaults.

Navigate to the `migrations` directory and build the utilities

    cd migrations
    stack build

Once `stack` is finished (this may take a little while), ensure that Postgres is
running on your system, initialize the database, run the migrations, and seed it 
with the default test user

    make init
    stack exec migrate -- -c './resources/refurb/config' migrate --seed --execute

This will create a `crw_db` database, a `crw_user` user to manage it, install the
`uuid-ossp` extension, perform the migrations outlined in `src/Main.hs`, and 
seed a user with the following credentials in the `users` table:

- email: "alice@crw.com"
- username: "Alice Roberts"
- password: "guest123"

These credentials will be used later to login and get a bearer token allowing 
access to protected endpoints.

Once this is complete, navigate back up to the main project directory.

#### Generate and Source the RSA Keyfile
This application uses an RSA Keyfile to generate the JSON Web Key (JWK) used
throughout the JWT generation and validation processs.

An RSA keyfile should be generated and stored in a location accessible to the
application. The filepath is assumed to be `./resources/keys/compositeRealWorld.key`, 
which is defined in the `.env` file by default under the `KEYPATH` variable.

Generate the key at a specified filepath

    ssh-keygen -t rsa -b 4096 -f ./resources/keys/compositeRealWorld.key
    
Ensure that the key filepath is properly described under `KEYPATH` in the `.env`
file.

#### Build and Run the Application

A set of default configuration settings has been provided in `./.env`; note that
if any of the settings related to the database or the RSA keyfile are changed,
their respective steps above must be re-run to conform to the new settings.

After that, it's relatively simple to build and run the application

    stack build
    stack exec composite-realworld-exe

By default, this runs the app in a `DEV` setting, which includes a pretty-printer
for HTTP requests courtesy of `wai-extra`.

Try `POST`ing to `localhost:8080/api/users/login` with the the seeded admin info
to verify that everything worked!

## Resources
A lot of this code has been copied, adapted, or at the very least least inspired
by the following examples/libraries:

- [Matt Parsons]' [servant-persistent]
- [Confer Health]'s [composite] 
- [Chris Allen] and [Alexey Zabelin]'s [cards-with-comrades]
- Example code from the [servant-auth] repository
- And possibly many more that have slipped my mind!

[servant-auth]: https://github.com/plow-technologies/servant-auth
[Confer Health]: https://github.com/ConferHealth
[composite]: https://github.com/ConferHealth/composite
[Matt Parsons]: https://github.com/parsonsmatt
[servant-persistent]: https://github.com/parsonsmatt/servant-persistent
[Chris Allen]: https://github.com/bitemyapp
[Alexey Zabelin]: https://github.com/alexeyzab
[cards-with-comrades]: https://github.com/alexeyzab/cards-with-comrades
