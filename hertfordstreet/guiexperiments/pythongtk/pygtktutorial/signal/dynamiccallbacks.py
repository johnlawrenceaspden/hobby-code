#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk

def destroy(widget, data=None):
    print "destroy event occurred", widget, data
    print "killing main loop"
    gtk.main_quit()

def paralyse(widget, data=None):
    b.disconnect(destroy_handler)
    b.set_label("Aargh, you deaded me!")
    b.show()

def block(widget, data=None):
    b.handler_block(destroy_handler)

def unblock(widget, data=None):
    b.handler_unblock(destroy_handler)

w=gtk.Window()
w.connect("destroy", destroy)

a=gtk.Button("temporarily paralyse")
aa=gtk.Button("unparalyse")
c=gtk.Button("permanently paralyses the other button")
b=gtk.Button("This button's click event is attached to the other button's destroy")

destroy_handler=b.connect_object("clicked", gtk.Widget.destroy, c)
c.connect("clicked", paralyse)
a.connect("clicked", block)
aa.connect("clicked", unblock) 

v=gtk.VBox()
v.add(a)
v.add(aa)
v.add(b)
v.add(c)
w.add(v)
w.show_all()

gtk.main()
