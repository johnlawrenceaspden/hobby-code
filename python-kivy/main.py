#!/usr/bin/env python 
from kivy.app import App #We need to import the bits of kivy we need as we need them as importing everything would slow the app down unnecessarily
from kivy.uix.widget import Widget #this is a thing that you want the App to display


class Lesson0(Widget): #this defines the instance of the widget. 
    pass # pass is used to keep the class valid but allow it not to contain anything - At the moment our widget is not defined.


class MyApp(App):
    def build(self):
        return Lesson0()

if __name__ == '__main__': #Documentation suggests that each program file should be called main.py but I think that only matters if you're creating the final App to go onto a phone or tablet we're a long way off from that yet

    MyApp().run() #This must match the name of your App
