#!/usr/bin/env python


# See:
# https://kivy.org/doc/stable/guide/basic.html
# https://nmilosev.svbtle.com/installing-kivy-on-fedora-29

#We need to import the bits of kivy we need as we need them as importing everything would slow the app down unnecessarily
from kivy.app import App 
from kivy.uix.widget import Widget 
from kivy.uix.label import Label 
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label
from kivy.uix.textinput import TextInput
from kivy.uix.button import Button
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.anchorlayout import AnchorLayout
from kivy.uix.switch import Switch
from kivy.uix.togglebutton import ToggleButton
from kivy.uix.floatlayout import FloatLayout
 
from kivy.core.audio import SoundLoader


clickcount=0

class LoginScreen(GridLayout):

    def __init__(self, **kwargs):
        super(LoginScreen, self).__init__(**kwargs)
        layout = BoxLayout(orientation='vertical')
        self.button = Button(text="Hard")
        self.button.bind(on_press=playsound)
        self.button2 = Button(text="Good")
        self.button2.bind(on_press=playsound)
        self.button3 = Button(text="Easy")
        self.button3.bind(on_press=playsound)


        self.cols = 1
        #order of addition is significant for widget placement
        self.add_widget(layout)
        layout.add_widget(Label(text='Music'))
        layout.add_widget(Label(text='Numbers\n135153151'))
        layout.add_widget(Label(text='Rhythm\nta ta-di ta ta-di ta ta ta - aa '))
        
        self.buttonlayout = BoxLayout()
        layout.add_widget(self.buttonlayout)

        self.buttonlayout.add_widget(self.button)
        self.buttonlayout.add_widget(self.button2)
        self.buttonlayout.add_widget(self.button3)

import time

def playsound(dummy):
    sound = SoundLoader.load('mytest.ogg')
    if sound:
        print("Sound found at %s" % sound.source)
        print("Sound is %.3f seconds" % sound.length)
        sound.play()
        print("sound state", sound.state)
        print("sleep")
        time.sleep(1)
        print("sleep")

        print("sound state", sound.state)
        sound.stop()
        sound.seek(0)
        print("sound state", sound.state)
        
        sound.pitch=5/4
        sound.play()
        
        print("sound state", sound.state)
        

class HelloWorldApp(App):
    def build(self):
        return LoginScreen()

if __name__ == '__main__': #Documentation suggests that each program file should be called main.py but I think that only matters if you're creating the final App to go onto a phone or tablet we're a long way off from that yet

    HelloWorldApp().run() #This must match the name of your App


