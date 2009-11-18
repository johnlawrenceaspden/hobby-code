#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk
import random

def callback(widget, data):
    print "Callback", data

def delete_event(widget, event, data=None):
    gtk.main_quit()
    return False

h=gtk.HBox()

b1=gtk.Button("Button 1")
b1.connect("clicked", callback, "1")

h.add(b1)


for padding in [0, 10]:
    for fill in [True, False]:
        for expand in [True, False]:
            name="expand %s\nfill %s\npadding %s" % (expand, fill, padding)
            ab=gtk.Button(name)
            ab.connect("clicked", callback, name)
            h.pack_start(ab,expand,fill,padding)

w=gtk.Window()
w.connect("delete_event", delete_event)
w.add(h)
w.set_border_width(10)

w.show_all()
gtk.main()
