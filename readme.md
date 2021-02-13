# A Sparql 1.1 Playground

This project is an experiment into an Elm-based front-end to a Sparql 1.1 compliant endpoint.  It is thoroughly tested against Jena Fuseki over the rest API.

The design philosophy recognizes that in general `select` queries return tabular data, while `construct` queries return graph data in the shape of `subject`, `predicate`, `object` triples.  

For the purpose of the UI, the graph results are pivoted in such a way that all `predicate/objects` for a `subject` are gathered onto a 'card'.  The reverse of the 'card' (`Back Links`) shows all places where the `subject` of the 'card' appears in the `object` position.


## Overall Layout

![](docs/architecture.png)

In this project Server.py is a Python Flask API that intercepts Sparql commands, forwards them to the endpoint and prepares the results for the front end.

## Prerequisites

- Apache Jena Fuseki.  Choose the latest release from the [Maven Repository](https://repo1.maven.org/maven2/org/apache/jena/jena-fuseki-server/).  This is a Java `jar` and so needs a working Java JRE or SDK.
- A Python 3.6+ environment.

## Shorthand queries