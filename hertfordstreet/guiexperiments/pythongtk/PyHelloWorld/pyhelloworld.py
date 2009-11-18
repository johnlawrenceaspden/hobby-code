#!/usr/bin/env python

import sys
import pygtk
pygtk.require("2.0")
import gtk
import gtk.glade

class HelloWorldGTK:
    """This is a Hello World GTK application"""
    def __init__(self):
        self.gladefile="pyhelloworld.glade"
        self.wTree = gtk.glade.XML(self.gladefile)

        dic={"on_btnHelloWorld_clicked": self.btnHelloWorld_clicked,
               "on_MainWindow_destroy" : gtk.main_quit}
        self.wTree.signal_autoconnect(dic)

    def btnHelloWorld_clicked(self,a):
        print "click!"
        print self
        print a


hwg = HelloWorldGTK()
gtk.main()
