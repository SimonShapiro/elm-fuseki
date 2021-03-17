# To Do

## Known Bugs

- [x] Scan jsonld for `null` value
- [x] Make error pages return to query screen Untested because it relates to APIError
- [x] Push url on submit
- [x] Remove hard coding of localhost 127.0.0.1 and port
- [x] Resolve apparent differences between safari and chrome on back from history after eternal url click
- [x] Improve methods of recognizing query type in api to fine tune headers
- [x] Resolve treatment of fragments in url's - [ ] there is a cut-off on the `#` on transfer to a server - [ ] this is a general browser problem - [ ] perhaps encode it as %23 or cut it off before `#` and then close the > for describe links - [ ] choose second method %23
- [x] Resolve differences between bnode on select and construct in server.py - [ ] one traces to b0 the other to \_:bo. Should probably fold blank nodes into there subject in some recursive function.
- [x] Add lru caching to improve query performance
- [x] Element.text word wrap of object literals
- [x] Element.Input.text loss of focus
- [x] Need to get server errors back to ui - [ ] especially related to update
- [x] A bug on `select distinct ?g {?s ?p ?o.}`
- [x] need to short the text on ultra-long joined up objects - [ ] esp. Url a
- [x] Paragraph does not always flow at end of box - [ ] currently only observed intermittently at work version???!!!
- [x] Increase vertical spacing on moleculeCard
- [x] make subject header respond to `terse/verbose`
- [x] Move pin to a general result area off the card - to indicate 'pin' the result.
- [x] query in editor on use of line of thought
- [x] Back isn't using the cache results
- [ ] Empty graph causes errors
- [x] Problem with graph drawing and history
- [ ] Store history/query cache data in session storage - this will impact performance
- [ ] Remove unnecessary `Debug.log` statements
- [ ] extra long unwrappables in ui - [ ] like tbl cert in `select * from <http://person.org/tbl> {?s ?p ?o}`
- [ ] `Ouput format` used in two places on sparql page
- [ ] replace error elements with paragraph elements.
- [ ] Command line query should not be submitted - could be fixed with session results cache
- [ ] Add port as command-line parameter for Server.py
- [ ] Fine tune cache maxsize
- [ ] Back reloads page and loses reference position

## Fine Tune Features

- [x] Cursor on wait
- [x] Table/subject view toggle
- [x] Provide for `terse` predicates
- [x] Bulk up predicates into lists
- [x] UI to accommodate lists
- [x] Elaborate on the structure of SelectAtom
- [x] Incorporate improved SelectAtom into useful ui interpretation
- [x] Make target fuseki a command parameter
- [x] Single subject molecule url
- [x] Links over single url views
- [x] Icon for fully external url lookup
- [x] Link on subject as well
- [x] Implement `load` in update
- [x] Updates according to - [sparql cheat sheet] (<https://www.iro.umontreal.ca/~lapalme/ift6281/sparql-1_1-cheat-sheet.pdf>)
- [x] Maintain scrolling query list
- [x] Investigate elm-ui
- [x] Simple styling
- [x] add reverse cards via query - [ ] ie back links
- [x] packaged queries through a range of ! commands
- [x] automatically sense non-s, p, o in subject view
- [x] render markdown literals [http://net.daringfireball.markdown, which conforms to public.plain-text. The encoding should always be UTF-8.](https://daringfireball.net/linked/2011/08/05/markdown-uti)
- [x] Cache results in model
- [x] Introduce refresh of cached query results - automatic on `drop`, `load`, and `insert`
- [x] Improve shape of query history by using `Element.textColumn`
- [x] Allow `Terse` on object position url's
- [x] graph view
- [x] implement pan and zoom
- [ ] Add limit and offset semantics in table and subject oriented list
- [ ] parameterize graph control attributes
- [ ] remove output graph in graphviz .DOT format
- [ ] Add settings - api url; allow implied external links to model
- [ ] Replace raw id's in ?s and ?o with prefLabel, if available.
- [ ] Consider doing something special with #type???
- [ ] see if there is a anything useful for `bnode` in the object position when showing `Back Links`
- [ ] make no info on subject view more obvious - by providing access to `Back Links`
- [ ] integrate ttl validator `npm install -g turtle-validator` - as a command in load logic
- [ ] find some way to sort lists
- [ ] improved text on back link predicates this involves a split and rejoin function which needs to be applied at display time only.  At this point we will need to distinguish modes on the moleculeCard.
- [ ] consider user defined short-cuts to queries like ! commands
- [ ] add load in addition to execute query icon on query history
- [ ] show/hide query history (use n more / less) semantics
- [ ] Improve url routing
- [ ] check for in result/graph access to object uri
- [ ] Save ttl in subject view
- [ ] Tableview to get hyperlink logic?
- [ ] Dockerise server.py
- [ ] Investigate local fuseki making federated queries.
- [ ] Investigate fuseki deployment/embedding and tdb sync
- [ ] csv download - [ ] filename to be input
- [ ] Improve table (a more fully featured table)

## Pending Re-factors

- [x] Replace Sparql.toString with PlaygroundQuery.toString
- [ ] use a decoder strategy for establishQueryType
- [ ] makeRdfDict redundant call and setting of table/subject

### Sparql Processing

- [x] Separate `Sparql` - [ ] confirm you have sparql in compatible shape - [ ] submit to end-point
- [x] Merge `submitParametrisedQuery` and `submitQuery`

### KG Response Processing

Transform the response into internal form(s).  Main logic is driven by functions leading to RdfDict.

- [x] Separate all functions from KGresponse to ContractedForm.
