# A Sparql 1.1 Playground
![](2021-02-14-11-16-29.png)
This project is an experiment into an Elm-based front-end to a Sparql 1.1 compliant endpoint.  It is thoroughly tested against Jena Fuseki over the rest API.

The design philosophy recognizes that in general `select` queries return tabular data, while `construct` queries return graph data in the shape of `subject`, `predicate`, `object` triples.  

For the purpose of the UI, the graph results are pivoted in such a way that all `predicate/objects` for a `subject` are gathered onto a 'card'.  The reverse of the 'card' (`Back Links`) shows all places where the `subject` of the 'card' appears in the `object` position in other triples.

## Overall Layout

![](docs/architecture.png)

In this project Server.py is a Python Flask API that intercepts Sparql commands, forwards them to the endpoint and prepares the results for the front end.

## Prerequisites

- Apache Jena Fuseki.  Choose the latest release from the [Maven Repository](https://repo1.maven.org/maven2/org/apache/jena/jena-fuseki-server/).  This is a Java `jar` and so needs a working Java JRE or SDK.  The latest version as at 2021-02-16 is 3.17.0, which is used below.  Use `curl` to get a copy as below:

```
curl --output <WHERE-YOU-WANT-TO-HOLD-FUSEKI> https://repo1.maven.org/maven2/org/apache/jena/jena-fuseki-server/3.17.0/jena-fuseki-server-3.17.0.jar
```

The server can be started with the following command:

```
java -Xmx2g -jar fuseki-server.jar --tdb2 --loc <WHERE-YOU-WANT-YOUR-DATABASE> --set tdb:unionDefaultGraph=true --update <THE-NAME-OF-YOUR-DATASET>
```
- A Python 3.6+ environment.

## Shortcut queries

There are a standard set of queries which I use a lot to investigate a Knowledge Graph.  These start with `!` and convert to the full query in the query window.  For example, `!size` converts to `select (count(*) as ?count) {?s, ?p ?o}`.