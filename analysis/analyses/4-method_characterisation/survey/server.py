from flask import Flask, render_template, json, jsonify, request
from pathlib import Path
import pandas as pd
app = Flask(__name__)

people = pd.read_csv("people.csv")

@app.route("/<key>")
def hello(key):
  if key in people["key"].tolist():
    return render_template('survey.html')
  else:
    return "Invalid key!"

@app.route("/<key>/post", methods=['POST'])
def post(key):
  answers = request.get_json()
  json.dump(answers, open("static/answers/" + key + ".json", "w"))
  return ""

@app.route("/<key>/get")
def get(key):
  category = people.ix[people.key == key].category.tolist()[0]

  data = {
    "questions":json.load(open("static/questions_" + category + ".json")),
    "answers":{}
  }

  if Path("static/answers/" + key + ".json").exists():
    data["answers"] = json.load(open("static/answers/" + key + ".json"))

  return jsonify(data)

@app.route("/<key>/thanks")
def thanks(key):
  return render_template('thanks.html')
