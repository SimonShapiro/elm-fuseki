from flask import Flask, Response, request
from flask_cors import CORS
import requests
import json
from pyld import jsonld
from dataclasses import dataclass
import pprint

@dataclass
class Atom:
    _type: str = ""
    value: str = ""

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
            atoms = [{"key": v, "value": bound[v]["value"] if bound.get(v) else ""} for v in vars]
            rows.append(atoms)
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
            "result": rows
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
        rows = [[{"key": "answer", "value": "true"}]]
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
            "result": rows
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
                predicateAtom = {"key": "p", "value": predicate}
                print(predicateAtom)
                if predicate != '@id':
                    for obj in subj[predicate]:
                        if predicate == '@type':
                                objectAtom = {"key": "o", "value": obj}
#                                print(f'>>{objectAtom}')
                        else:
                            objectAtom = {"key": "o", "value": obj.get("@value") or obj.get("@id")}
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