# KVDB over DNS

This library provide a simple API to manage Key-Value query to a Data-Base
over DNS (using TXT queries).

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
    * "db" query: ask to access something in the database (try the keys "linux" or "haskell")
* Server:
    * respond to "echo" or "db" queries
    * return ServerError in case of base queries or non-TXT request

    cabal configure -f executable

# The API

## Request

All the query MUST implement the class **Encodable** (see Network.DNS.KVDB.Types).

The idea is to allow user to implement their own representation of a query.
For example, Network.DNS.KVDB.Types implements a **Request Encodable**.

    data Request = Request
        { domain :: ByteString
        , cmd    :: ByteString
        , nonce  :: ByteString
        , param  :: ByteString
    }

An encodable data MUST implement two methods:
* encode: converts the data into a valid FQDN
* decode: converts a FQDN into the needed data

In the case of a Request the FQDN will look like:

    <encoded32(param)>.<encoded32(nonce)>.<encoded32(cmd)>.domain

## Response

    data Response = Response
        { signature :: ByteString
        , response  :: ByteString
        }

Every response to a valide request will be return in this structure.

## The query methods

Let to user wishes.
