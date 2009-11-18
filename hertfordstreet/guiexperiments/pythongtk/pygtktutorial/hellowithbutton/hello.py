#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk

def clicked(widget, data=None):
    print "hello", widget, data

def destroy(widget, data=None):
    print "destroy event occurred", widget, data
    print "killing main loop"
    gtk.main_quit()
    
b=gtk.Button("Hello")
b.connect("clicked", clicked)

w=gtk.Window()
w.set_title("Hello with a button")
w.connect("destroy", destroy)
w.add(b)
w.show_all()

gtk.main()
