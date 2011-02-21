#!/usr/bin/env python
from flask import Flask

app=Flask(__name__)

@app.route("/")
def hello():
    return "Hell!"

@app.route("/doom")
def doom():
    return "Horror & Doom!"

if __name__=="__main__":
    app.run(debug=True)
