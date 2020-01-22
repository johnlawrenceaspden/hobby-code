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
        self.username = TextInput(multiline=False)
        self.password = TextInput(password=True, multiline=False)
        self.button = Button(text="Click Me.")
        self.button.bind(on_press=playsound)


        self.cols = 2
        #order of addition is significant for widget placement
        self.add_widget(Label(text='User Name'))
        self.add_widget(self.username)
        self.add_widget(Label(text='password'))
        self.add_widget(self.password)
        self.add_widget(self.button)

import time

def playsound(dummy):
    sound = SoundLoader.load('mytest.ogg')
    if sound:
        print("Sound found at %s" % sound.source)
        print("Sound is %.3f seconds" % sound.length)
        sound.play()
        print("sound state", sound.state)
        print("sleep")
        time.sleep(4)
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


