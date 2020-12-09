from flask import Flask 
from flask_cors import CORS

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
    return 500

if __name__ == "__main__":
    app.run()