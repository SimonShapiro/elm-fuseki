from flask import Flask, Response, request
from flask_cors import CORS
import requests
import json
from dataclasses import dataclass

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
    return "Hello"

@app.route("/sparql", methods=['POST'])
def sparql():
    queryString = request.data.decode()
    print(queryString)
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
    return Response(
        status = 200,
        response = json.dumps({
            "status": status,
            "reason": error,
            "queryType": "select",
            "query": queryString,
            "vars": vars,
            "result": rows
        })
    )

if __name__ == "__main__":
    app.run()