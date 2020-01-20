#!/usr/bin/env python

# tell it to use a different text renderer (from python image library), makes text worse.
import os
# really? we communicate with kivy by modifying the environment??
os.environ['KIVY_TEXT'] = 'pil'
import kivy

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

clickcount=0

class LoginScreen(GridLayout):

    def __init__(self, **kwargs):
        super(LoginScreen, self).__init__(**kwargs)
        self.username = TextInput(multiline=False)
        self.password = TextInput(password=True, multiline=False)
        self.button = Button(text="Click Me.")
        self.button.bind(on_press=self.displayMessage)
        self.subbox=kivy.uix.boxlayout.BoxLayout(orientation="vertical")
        self.anchor=AnchorLayout(anchor_x='center', anchor_y='center')


        self.label = Label(text="Not logged in. ("+str(clickcount)+")")
        self.ud =    Label(text="--")
        self.pd =    Label(text="--")

        self.sw = Switch(active=True)

        self.cols = 2
        #order of addition is significant for widget placement
        self.add_widget(Label(text='User Name'))
        self.add_widget(self.username)
        self.add_widget(Label(text='password'))
        self.add_widget(self.password)
        self.add_widget(self.button)
        self.add_widget(self.subbox)
        self.add_widget(self.anchor)
        
        self.subbox.add_widget(self.label)
        self.subbox.add_widget(self.ud)
        self.subbox.add_widget(self.pd)

        self.anchor.add_widget(self.sw)

    def displayMessage(self, btn):
        global clickcount
        clickcount+=1
        self.label.text = "that's close enough for government work ("+str(clickcount)+")"
        self.ud.text=self.username.text
        self.pd.text=self.password.text



class HelloWorldApp(App):
    def build(self):
        return LoginScreen()

if __name__ == '__main__': #Documentation suggests that each program file should be called main.py but I think that only matters if you're creating the final App to go onto a phone or tablet we're a long way off from that yet

    HelloWorldApp().run() #This must match the name of your App


## just dropping off the end will do, but want to keep scratch code below
exit()
## dead code with any luck
assert(False)

    
    
class SimpleApp(kivy.app.App):
    def build(self):
        self.textInput = kivy.uix.textinput.TextInput()
        self.label = kivy.uix.label.Label(text="Your Message.")
        self.button = kivy.uix.button.Button(text="Click Me.")
        self.button.bind(on_press=self.displayMessage)
        self.boxLayout = kivy.uix.boxlayout.BoxLayout(orientation="vertical")
        self.boxLayout.add_widget(self.textInput)
        self.boxLayout.add_widget(self.label)
        self.boxLayout.add_widget(self.button)
        return self.boxLayout

    def displayMessage(self, btn):
        self.label.text = self.textInput.text

if __name__ == "__main__":
    simpleApp = SimpleApp()
    simpleApp.run()



