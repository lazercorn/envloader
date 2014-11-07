envloader
=========

This is an attempt at getting Erlang support for loading .env files for configuring 
the system environment within an Erlang app.

Envloader uses the same regex for parsing .env files as the the original [dotenv](https://github.com/bkeepers/dotenv) Ruby library.

## Usage

Just call `load/1` on the `envloader` module with a path to the .env file:

    envloader:load("/path/to/.env").

## Forthcoming

* automatically load .env on startup (envloader may become a gen_server)
* no configuration necessary, just include envloader in the app start sequence
