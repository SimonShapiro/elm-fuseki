from flask import Flask, Response, request
from flask_cors import CORS
import requests
import json
from pyld import jsonld
from dataclasses import dataclass
from dataclasses_json import dataclass_json
import pprint
from optparse import OptionParser
import re

@dataclass_json
@dataclass
class Atom:
    key: str
    value: str
    aType: str
    datatype: str = ""
    language: str = ""


app = Flask(__name__)
app.config['API_TITLE'] = 'My API'
app.config['API_VERSION'] = 'v1'
app.config['OPENAPI_VERSION'] = '3.0.2'

CORS(app)

SERVER = ""#"http://localhost:3030/latest"

@app.route('/hello', methods=['GET'])
def hello():
    print(request.url_root[:-1])
    return "Hello"

def buildAtom(v, bound):
    # standardise shape format of bnode to _:
    if bound[v]["type"] == 'bnode':
        bound[v]["value"] = "_:"+bound[v]["value"]
    atom = Atom(key=v, 
                value=bound[v]["value"] if bound.get(v) else "",
                aType=bound[v]["type"]
            )
    if bound[v].get("xml:lang"):
        atom.language = bound[v].get("xml:lang")
    if bound[v].get("datatype"):
        atom.datatype = bound[v].get("datatype")
    return atom

def serializeResults(rows):
    return [[a.to_dict() for a in r] for r in rows ]

def stringifyValue(value):
    if value != None:
        if not isinstance(value, str):
            return str(value)
        else:
            return value
    else:
        None

def rdfTypeSignature(value):
    prefix = "http://www.w3.org/2001/XMLSchema#"
    if isinstance(value, int):
        return prefix+"integer"
    elif isinstance(value, float):
        return prefix+"decimal"
    elif isinstance(value, bool):
        return prefix+"boolean"

def processSelectQuery(qType, queryString, res):
    print(res.status_code)
    print(res.text)
    status = res.status_code
    error = res.reason
    if res.status_code == 200:
        result = res.json()  # only works for `ask` and `select`
        vars = result["head"]["vars"]
        bindings = result["results"]["bindings"]
        rows = []
        for bound in bindings:
            # create atom here
            atoms = [buildAtom(v, bound) for v in vars]
            rows.append(atoms)
    else:
        result = {}
        vars = []
        rows = []
    print(serializeResults(rows))
    return {
        "status": 200,
        "response": json.dumps({
            "server": request.url_root[:-1],
            "status": status,
            "reason": error,
            "queryType": qType,
            "query": queryString,
            "vars": vars,
            "result": serializeResults(rows) 
        })}

def processAskQuery(qType, queryString, res):
    print(res.status_code)
    print(res.text)
    status = res.status_code
    error = res.reason
    if res.status_code == 200:
        result = res.json()  # only works for `ask` and `select`
        print(result)
        vars = ["answer"]
        rows = [[Atom(key="answer", 
                        value=str(result["boolean"]), 
                        aType="literal", 
                        datatype="http://www.w3.org/2001/XMLSchema#boolean"
                    )
                ]]
    else:
        result = {}
        vars = []
        rows = []
    print(status, error, vars, rows)
    return {
        "status": 200,
        "response": json.dumps({
            "server": request.url_root[:-1],
            "status": status,
            "reason": error,
            "queryType": qType,
            "query": queryString,
            "vars": vars,
            "result": serializeResults(rows)
        })}

def processConstructQuery(qType, queryString, res):
    print("Going for construct query", queryString)
    print(res.status_code)
    print(res.text)
    status = res.status_code
    error = res.reason
    if res.status_code == 200:
        result = res.json()  # only works for `ask` and `select`
#        result = [{"@id":x['@id']} for x in jsonld.expand(result)]
        result = jsonld.expand(result)
        pprint.pprint(result)
        resultArray= []
        for subj in result:
            subjectAtom =  Atom(key="s", 
                                value=subj['@id'], 
                                aType="uri"
                            ) if not ("_:" in subj['@id']) else Atom(key="s", 
                                value=subj['@id'], 
                                aType="bnode"
                            )# decoding aType into bnode as well 
            # this loop yields predicates
            for predicate in subj.keys():
                predicateAtom = Atom(key="p", 
                                        value=predicate if predicate != "@type" else "http://www.w3.org/2000/01/rdf-schema#type",
                                        aType="uri"
                                    )
                print(predicateAtom)
                if predicate != '@id':
                    for obj in subj[predicate]:
                        if predicate == '@type':
                            objectAtom = Atom(key="o",
                                                value=obj if obj else "",
                                                aType="uri"
                                                )
                        else:  # need to be careful with sensing difference between @value and @id
                            # use rdfType where not explicity provided in json-ld
                            print(obj)
                            if obj.get("@value") is not None:
                                objectAtom = Atom(key="o", 
                                                    value=stringifyValue(obj.get("@value")),
                                                    aType= 'literal'
                                                )
                                if obj.get("@language"):
                                    objectAtom.language = obj.get("@language")
                                if rdfTypeSignature(obj.get("@value")):
                                    objectAtom.datatype = rdfTypeSignature(obj.get("@value"))
                            elif obj.get("@id"): 
                                objectAtom = Atom(key="o", 
                                                    value=obj.get("@id"),
                                                    aType= "uri"
                                                ) if not "_:" in obj['@id'] else Atom(key="s", 
                                                        value=obj['@id'], 
                                                        aType="bnode"
                                                    )
                            else:
                                print("Unexpected empty object in triple")
                                print(1/0)
#                            print(f'--{objectAtom}')
#                        print([subjectAtom, predicateAtom, objectAtom])
                        resultArray.append([subjectAtom, predicateAtom, objectAtom])
    else:
        pass
    print(serializeResults(resultArray))
    return {
        "status": 200,
        "response": json.dumps({
            "server": request.url_root[:-1],
            "status": status,
            "reason": error,
            "queryType": qType,
            "query": queryString,
            "vars": ['s', 'p', 'o'],
            "result": serializeResults(resultArray)
        })}

def establishQueryType(queryString):
    q = queryString.lower()
    selectRe = r"^select"
    askRe = r"^ask"
    constructRe = r"^construct"
    describeRe = r"^describe"
    if re.search(selectRe, q, re.M):
        return "select"
    elif re.search(askRe, q, re.M):
        return "ask"
    elif re.search(constructRe, q, re.M):
        return "construct"
    elif re.search(describeRe, q, re.M):
        return "describe"
    else:
        return "select"  #  might need something else here

@app.route("/sparql", methods=['POST'])
def sparql():
    queryString = request.data.decode()
    print(queryString)
    print(request.headers)
    clientAcceptHeader = request.headers["Accept"] if request.headers.get("Accept") else "application/json"
    res = requests.request("POST", SERVER+"/sparql",
            data = queryString, 
            headers = {
                'Content-Type': 'application/sparql-query',
                'Accept': clientAcceptHeader
                })
    qType = establishQueryType(queryString)  # request.headers.get("X-Qtype")  # replace with python function to remove reliaze on headers
    if  qType == "select":
        result = processSelectQuery(qType, queryString, res)
        return Response(
            status = result["status"],
            response = result["response"]
        )
    elif  qType == "ask":
        result = processAskQuery(qType, queryString, res)
        return Response(
            status = result["status"],
            response = result["response"]
        )
    elif (qType == "construct") or (qType == "describe"):
        print("Constrcuting a response")
        result = processConstructQuery(qType, queryString, res)
        return Response(
            status = result["status"],
            response = result["response"]
        )

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-s", "--server", dest="server",
                    help="Location of fuseki server", metavar="SERVER")
    # parser.add_option("-q", "--quiet",
    #                 action="store_false", dest="verbose", default=True,
    #                 help="don't print status messages to stdout")
    (options, args) = parser.parse_args()
    print(options, args)
    SERVER = options.server
    app.run()