#!/usr/bin/env python
from flask import Flask

app=Flask(__name__)

@app.route("/")
def hello():
    return "Hell!"

@app.route("/doom")
def doom():
    return "Horror & Doom!"

@app.route('/user/<username>')
def show_user_profile(username):
    return username

@app.route('/post/<int:post_id>')
def show_post(post_id):
    return str(post_id)



if __name__=="__main__":
    app.run(debug=True)
