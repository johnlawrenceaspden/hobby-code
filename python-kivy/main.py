#!/usr/bin/env python 
from kivy.app import App #We need to import the bits of kivy we need as we need them as importing everything would slow the app down unnecessarily
from kivy.uix.widget import Widget #this is a thing that you want the App to display
from kivy.uix.label import Label #this will import the code for the label in which we want to display Hello World! 


class Lesson1App(App):
    def build(self):
        lbl=Label(text='Hello World!') #lbl is a variable name being assigned the Label definition
        return lbl #This  must match the name of the Widget you want to appear on screen

if __name__ == '__main__': #Documentation suggests that each program file should be called main.py but I think that only matters if you're creating the final App to go onto a phone or tablet we're a long way off from that yet

    Lesson1App().run() #This must match the name of your App
