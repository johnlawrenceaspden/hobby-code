#!/usr/bin/env python 
from kivy.app import App #We need to import the bits of kivy we need as we need them as importing everything would slow the app down unnecessarily
from kivy.uix.widget import Widget #this is a thing that you want the App to display
from kivy.uix.label import Label #this will import the code for the label in which we want to display Hello World! 
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label
from kivy.uix.textinput import TextInput

class LoginScreen(GridLayout):

    def __init__(self, **kwargs):
        super(LoginScreen, self).__init__(**kwargs)

        
        self.username = TextInput(multiline=False)
        self.password = TextInput(password=True, multiline=False)

        self.cols = 2
        #order of addition is significant for widget placement
        self.add_widget(Label(text='User Name'))
        self.add_widget(self.username)
        self.add_widget(Label(text='password'))
        self.add_widget(self.password)




class HelloWorldApp(App):
    def build(self):
        return LoginScreen()

if __name__ == '__main__': #Documentation suggests that each program file should be called main.py but I think that only matters if you're creating the final App to go onto a phone or tablet we're a long way off from that yet

    HelloWorldApp().run() #This must match the name of your App


