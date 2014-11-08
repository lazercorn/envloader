envloader
=========

This is an attempt at getting Erlang support for loading .env files for configuring 
the system environment within an Erlang app.

Envloader uses the same regex for parsing .env files as the the original [dotenv](https://github.com/bkeepers/dotenv) Ruby library.

## Usage

    application:start(envloader).

The .env in the current working directory will be automatically loaded.
