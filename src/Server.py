from flask import Flask, Response, request
from flask_cors import CORS
import requests
import json
from pyld import jsonld
from dataclasses import dataclass
from dataclasses_serialization.json import JSONSerializer
import pprint

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

SERVER = "http://localhost:3030/latest"

@app.route('/hello', methods=['GET'])
def hello():
    print(request.url_root[:-1])
    return "Hello"

def buildAtom(v, bound):
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
    return [[JSONSerializer.serialize(a) for a in r] for r in rows ]

def stringifyValue(value):
    if not isinstance(value, str):
        return str(value)
    else:
        return value

def processSelectQuery(queryString):
    res = requests.request("POST", SERVER+"/sparql",
            data = queryString, 
            headers = {
                'Content-Type': 'application/sparql-query',
                'Accept': 'application/json'
                })
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
            "queryType": "select",
            "query": queryString,
            "vars": vars,
            "result": serializeResults(rows) 
        })}

def processAskQuery(queryString):
    res = requests.request("POST", SERVER+"/sparql",
            data = queryString, 
            headers = {
                'Content-Type': 'application/sparql-query',
                'Accept': 'application/json'
                })
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
            "queryType": "select",
            "query": queryString,
            "vars": vars,
            "result": serializeResults(rows)
        })}

def processConstructQuery(queryString):
    res = requests.request("POST", SERVER+"/sparql",
            data = queryString, 
            headers = {
                'Content-Type': 'application/sparql-query',
                'Accept': 'application/ld+json'
                })
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
            subjectAtom = {"key": "s", "value": subj['@id']}
#            print(f'subject={subjectAtom}')
#            print('+++++++++++++')
            # this loop yields predicates
            for predicate in subj.keys():
                predicateAtom = {"key": "p", "value": predicate if predicate != "@type" else "http://www.w3.org/2000/01/rdf-schema#type"}
                print(predicateAtom)
                if predicate != '@id':
                    for obj in subj[predicate]:
                        if predicate == '@type':
                                objectAtom = {"key": "o", "value": obj if obj else ""}
#                                print(f'>>{objectAtom}')
                        else:
                            objectAtom = {"key": "o", "value": stringifyValue(obj.get("@value")) or obj.get("@id") or ""}
#                            print(f'--{objectAtom}')
#                        print([subjectAtom, predicateAtom, objectAtom])
                        resultArray.append([subjectAtom, predicateAtom, objectAtom])
    else:
        pass
    return {
        "status": 200,
        "response": json.dumps({
            "server": request.url_root[:-1],
            "status": status,
            "reason": error,
            "queryType": "select",
            "query": queryString,
            "vars": ['s', 'p', 'o'],
            "result": resultArray
        })}

@app.route("/sparql", methods=['POST'])
def sparql():
    queryString = request.data.decode()
    print(queryString)
    if "select" in queryString.lower():
        result = processSelectQuery(queryString)
        return Response(
            status = result["status"],
            response = result["response"]
        )
    elif "ask" in queryString.lower():
        result = processAskQuery(queryString)
        return Response(
            status = result["status"],
            response = result["response"]
        )
    elif ("construct" in queryString.lower()) or ("describe" in queryString.lower()):
        result = processConstructQuery(queryString)
        return Response(
            status = result["status"],
            response = result["response"]
        )
    
if __name__ == "__main__":
    app.run()