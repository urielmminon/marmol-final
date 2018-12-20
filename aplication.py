from flask import Flask, url_for, jsonify, request, json
import pickle
import numpy as np

with open("model3.pkl", "rb") as f:
	model = pickle.load(f)

app = Flask(__name__)

@app.route('/', methods=['POST'])

def predict():
	if request.header['Content-Type'] == 'application/json':
		input=reques.json['input']
	results = {}
	y_hat = model.predict(input)
	results['parameters']= input
	results['resultado'] = y_hat.tolist()
	return jsonify(results)

if __name__ == '__main__':
	app.run(port=5000)
