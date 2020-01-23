#!/usr/bin/env python

# https://blog.kivy.org/2019/12/kivy-tutorial-001-say-hello/

# See:
# https://kivy.org/doc/stable/guide/basic.html
# https://nmilosev.svbtle.com/installing-kivy-on-fedora-29


from kivy.app import App

from kivy.uix.label import Label

class HelloWorldApp(App):
    def build(self):
        root_widget = Label()
        root_widget.text = 'Hello world!'
        root_widget.font_size='100sp'
        return root_widget

HelloWorldApp().run()
