#!/usr/bin/env python

# https://blog.kivy.org/2019/12/kivy-tutorial-001-say-hello/

# See:
# https://kivy.org/doc/stable/guide/basic.html
# https://nmilosev.svbtle.com/installing-kivy-on-fedora-29
from kivy.app import App
from kivy.uix.button import Button
from kivy.uix.boxlayout import BoxLayout
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label

import inspect

def print_button_text(*args, **kwargs):
    print("args->",args)
    print("kwargs->", kwargs)
    print("locals()->", locals())
    instance=args[0]
    print("locals()->", locals())
    print("from global", instance.text)
    print("   ",inspect.getargspec(YourApp.print_button_text))
    print("   ",inspect.getargspec(print_button_text))



class YourApp(App):
    def build(self):
        root_widget = BoxLayout(orientation='vertical')

        output_label = Label(size_hint_y=1)

        button_symbols = ('1', '2', '3', '+',
                          '4', '5', '6', '-',
                          '7', '8', '9', '.',
                          '0', '*', '/', '=')

        button_grid = GridLayout(cols=4, size_hint_y=2)
        for symbol in button_symbols:
            button_grid.add_widget(Button(text=symbol))

        clear_button = Button(text='clear', size_hint_y=None,
                              height=100)

        root_widget.add_widget(output_label)
        root_widget.add_widget(button_grid)
        root_widget.add_widget(clear_button)

        for i,button in enumerate(button_grid.children[1:]):  # note use of the `children` property
            button.bind(on_press=self.print_button_text if i%2 else print_button_text)



            
        
        return root_widget

#    def print_button_text(self, instance):
    def print_button_text(*args, **kwargs):
        print("args->",args)
        print("kwargs->", kwargs)
        print("locals()->", locals())
        self=args[0]
        instance=args[1]
        print("locals()->", locals())
        print("from app:", instance.text)
        print("   ",inspect.getargspec(self.print_button_text))
        print("   ",inspect.getargspec(print_button_text))




YourApp().run()
