#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk

def destroy(widget, data=None):
    print "destroy event occurred", widget, data
    print "killing main loop"
    gtk.main_quit()

w=gtk.Window()
w.connect("destroy", destroy)

c=gtk.Button("This button's click event is attached to the main window's destroy")
b=gtk.Button("This button's click event is attached to the other button's destroy")

b.connect_object("clicked", gtk.Widget.destroy, c)
c.connect_object("clicked", gtk.Widget.destroy, w)

v=gtk.VBox()
v.add(b)
v.add(c)
w.add(v)
w.show_all()

gtk.main()
