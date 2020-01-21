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

        self.floater=BoxLayout()
        self.toggle=ToggleButton(text="togglebutton", font_size=20)

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
        self.add_widget(self.floater)
        
        self.subbox.add_widget(self.label)
        self.subbox.add_widget(self.ud)
        self.subbox.add_widget(self.pd)

        self.anchor.add_widget(self.sw)
        
        self.floater.add_widget(self.toggle)

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


