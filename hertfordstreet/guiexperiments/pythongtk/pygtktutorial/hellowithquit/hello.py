#!/usr/bin/env python

import pygtk
pygtk.require('2.0')
import gtk



def delete_event(widget, event, data=None):
    print "delete event occurred", widget, event, data
    import random
    if random.randrange(4)==0:
        print "permitting deletion"
        return False
    else:
        print "ignoring deletion"
        return True

def destroy(widget, data=None):
    print "destroy event occurred", widget, data
    print "killing main loop"
    gtk.main_quit()

w=gtk.Window()
w.set_title("Hard to destroy")
w.connect("delete_event", delete_event)
w.connect("destroy", destroy)

l=gtk.Label("""
A demonstration of the difference between delete and destroy events.

When the close button is clicked, the window recieves a delete event from the window manager,
which is connected to the delete_event function. This then randomly decides whether to swallow it or not.
If it is not swallowed, then it propagates and causes the system to send a destroy event (watch standard output).

Destroy events are connected to the destroy function, which kills the event loop using gtk.main_quit(), terminating the program.
If you don't do this, the window is destroyed, but the main loop is still running and the interpreter needs to be killed manually.
""")
w.add(l)

w.show_all()
gtk.main()
