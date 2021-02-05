# Known Bugs

- ~~Scan jsonld for `null` value~~
- ~~Make error pages return to query screen~~ Untested because it relates to APIError
- ~~Push url on submit~~
- ~~Remove hard coding of localhost 127.0.0.1 and port~~
- ~~Resolve apparent differences between safari and chrome on back from history after eternal url click~~
- ~~Improve methods of recognising query type in api to fine tune headers~~
- ~~Resolve treatment of fragments in url's - there is a cut-off on the # on transfer to a server - this is a general browser problem - perhaps encode it as %23 or cut it off before # and then close the > for describe links - choose second method %23~~
- ~~Resolve differences between bnode on select and contruct in server.py - one traces to b0 the other to \_:bo. Should probably fold blank nodes into there subject in some recursive function.~~
- ~~Add lru caching to improve query performance~~
- Element.text word wrap of object literals
- Element.Input.text loss of focus
- Element
- Command line query should not be submitted
- Add message to `load' response
- Add port as commandline paramter for Server.py 
- A bug on `select distinct ?g {?s ?p ?o.}`
- Fine tune chache maxsize
- Back reloads page and loses reference position 

# To Do

- ~~Cursor on wait~~
- ~~Table/subject view toggle~~
- ~~Provide for `terse` predicates~~
- ~~Bulk up predicates into lists~~
- ~~UI to accomodate lists~~
- ~~Elaborate on the structure of SelectAtom~~
- ~~Incorporate improved SelectAtom into useful ui interpretation~~
- ~~Make target fuseki a command parameter~~
- ~~Single subject molecule url~~
- ~~Links over single url views~~
- ~~Icon for fully external url lookup~~
- ~~Link on subject as well~~
- ~~Implement `load` in update~~
- ~~Updates according to~~ [sparql cheat sheet](https://www.iro.umontreal.ca/~lapalme/ift6281/sparql-1_1-cheat-sheet.pdf)
- ~~Maintain scrolling query list~~
- ~~Investigate elm-ui~~
- ~~Simple styling~~
- show/hide query history (use n more / less) semantics
- Improve url routing
- Add settings - api url; allow implied external links to model
- check for in result/graph access to object uri
- Consider doing something special with #type???
- Save ttl in subject view
- Tableview to get hyperlink logic?
- Dockerise server.py
- graph view
- Add limit and offset semantics in table and subject oriented list
- csv download - filename to be input
- Improve table (a more fully featured table)

# Pending Re-factors

- Make DisplaySelectResults flow from RdfNode rather than SelectAtom

*Sparql Processing*
- ~~Separate `Sparql` - confirm you have sparql in compatible shape - submit to end-point~~
- ~~Merge `submitParametrisedQuery` and `submitQuery`~~

*KG Response Processing*

Tranform the response into internal form(s).  Main logic is driven by functions leading to RdfDict.
- ~~Separate all functions from KGresponse to ContractedForm.~~