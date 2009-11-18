#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk

def clicked(widget, data=None):
    print "hello", widget, data
    l=gtk.Label("hello "+data)
    v2.add(l)
    w.show_all()

def destroy(widget, data=None):
    print "destroy event occurred", widget, data
    print "killing main loop"
    gtk.main_quit()
    
b=gtk.Button("Hello John")
b.connect("clicked", clicked, 'John')

c=gtk.Button("Hello Fred")
c.connect("clicked", clicked, 'Fred')

v=gtk.VBox()
v.add(b)
v.add(c)

v2=gtk.VBox()

h=gtk.HBox()
h.add(v)
h.add(v2)

w=gtk.Window()
w.set_title("Hello with buttons")
w.connect("destroy", destroy)
w.add(h)
w.show_all()

gtk.main()
