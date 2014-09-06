# API over DNS

This library provide a simple API to send request using DNS protocol (the TXT query record)

It does not implement the DNS protocol, it uses the [dns](https://hackage.haskell.org/package/dns).

# Installation

## The library

Assuming that the Haskell compiler *GHC* and the Haskell build tool *cabal* is
already installed run the following command from the shell:

    cabal install

## Build the example

In the examples directory:
* Client:
    * "echo" query: ask to return what I send to him
    * "db" query: ask to access something in a database (try the keys "linux" or "haskell")
* Server:
    * respond to "echo" or "db" queries
    * return ServerError in case of bad queries or non-TXT request

    cabal configure -f executable

# The API

## Request

### Encodable

The DNS-API provides an interface to send query. A query MUST implements the
Class **Encodable**.

An encodable data MUST implement two methods:
* encode: converts the data into a valid FQDN
* decode: converts a FQDN into the needed data

### The default Encodable type

You can implement your own type of Request.
Network.DNS.API.Types already implements a **Request** **Encodable**:

    data Request p = Request
        { domain :: ByteString
        , cmd    :: p
        , nonce  :: ByteString
        }

*Where p is an instance of **Packable** (see below).*

In the case of the **Request p** the encoded command will look like:

    <encoded32(command and nonce)>.domain

### Packable

For those who want to use the default **Request** type, there is only one
thing to do: **Implements the commands**.



## Response

    data Response = Response
        { signature :: ByteString
        , response  :: ByteString
        }

Every response to a valide request will be return in this structure.

## The query methods

Let to user wishes.
