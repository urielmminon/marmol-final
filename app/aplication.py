from flask import Flask, url_for, jsonify, request, json
import pickle
import numpy as np

with open("/home/hector/marmol-final/model3.pkl", "rb") as f:
	model = pickle.load(f)

app = Flask(__name__)

@app.route('/api', methods=['POST'])

def predict():
	data = request.get_json(force=True)
	results = {}
	y_hat = model.predict(np.array(data['exp']))
	results['parameters']= data['exp']
	results['resultado'] = y_hat.tolist()
	return jsonify(results)

if __name__ == '__main__':
	app.run(debug=True, port=5000)
