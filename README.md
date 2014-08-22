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
    * perform Dummy query
* Server:
    * echo the dummy key (drop the domain)
    * forward all the non-TXT queries to a realDNS

    cabal configure -f executable

# The API

## Domain Encoding

All the query MUST implement the class **Encodable** (see Network.DNS.KVDB.Types).

The idea is to allow user to implement their own representation of a query.
For example, Network.DNS.KVDB.Types implement a **Dummy Encodable**.

    data Dummy = Dummy
        { domainServer :: String
        , key          :: String
    }

An encodable data MUST implement two methods:
* encode: converts the data into a valid FQDN
* decode: converts a FQDN into the needed data

## The query methods

Let to user wishes.
