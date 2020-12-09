from flask import Flask, Response, request
from flask_cors import CORS
import requests

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
    status = res.status_code
    if res.status_code == 200:
        result = res.json()
        error = res.reason
    else:
        result = {}
        error = res.reason
    print(status, error, result)
    return Response(
        status = 500
    )

if __name__ == "__main__":
    app.run()