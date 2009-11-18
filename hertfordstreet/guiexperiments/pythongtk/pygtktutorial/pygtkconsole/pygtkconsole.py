#!/usr/bin/env python
# -*- Mode: python; c-basic-offset: 4 -*-
#
# Interactive PyGtk Console, Johan Dahlin 2002
#

import os
import signal
import sys
import string
import socket, select
from code import InteractiveInterpreter, InteractiveConsole

import gtk
import gobject

# For compatibility, instead of using GDK.INPUT_READ or
# gtk.gdk.INPUT_READ depending on the PyGtk version
GDK_INPUT_READ = 1

class Mainloop(InteractiveInterpreter):
    def __init__(self, read_fd, sock):
        InteractiveInterpreter.__init__(self)
        self._rfd = os.fdopen(read_fd, 'r')
        self._sock = sock
        gobject.io_add_watch(read_fd, GDK_INPUT_READ, self.input_func)

    def read_message(self):
        length = ord(self._rfd.read(1))
        return self._rfd.read(length)

    def input_func(self, fd, cond):
        data = self.read_message()
        more = self.runsource(data)
        self._sock.send(chr(more))
        return True

    def run(self):
        gtk.main()
        
class Console(InteractiveConsole):
    def __init__(self, write_fd, sock, pid):
        InteractiveConsole.__init__(self)
        self._wfd = os.fdopen(write_fd, 'w')
        self._sock = sock
        self.pid = pid

    def send_message(self, message):
        self._wfd.write('%c%s' % (len(message), message))
        self._wfd.flush()

    def interact(self, banner=None):
        InteractiveConsole.interact(self, banner)
        # Die child die
        os.kill(self.pid, 9)
        
    def runsource(self, source, filename):
        self.send_message(source)
        # wait for notification from parent
        select.select([self._sock],[],[])
        more = ord(self._sock.recv(1))
        return more
        
class GtkInterpreter(Console):
    def __init__(self):
        rfd, wfd = os.pipe()
        # set up socket for returning command result
        sigsock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock_addr = None
        for port in range(4321,5321):
            try:
                sigsock.bind(('', port))
                sock_addr = ('', port)
            except:
                pass
        if not sock_addr:
            print "Can't open socket"
        sigsock.listen(1)

        parent_pid = os.getpid()
        child_pid = os.fork()
        if not child_pid:
            # connect to command return socket
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(sock_addr)
            g = Mainloop(rfd, sock)
            g.run()
        else:
            # Wait for command return socket connection
            sock, addr = sigsock.accept()
            Console.__init__(self, wfd, sock, child_pid)

def interact():
    try:
        import readline
        import rlcompleter
        readline.parse_and_bind('tab: complete')
    except ImportError:
        pass
    
    gi = GtkInterpreter()
    gi.push("from gtk import *")

    python_version = string.split(sys.version)[0]
    try:
	pygtk_version = string.join(map(str, gtk.pygtk_version), '.')
	gtk_version = string.join(map(str, gtk.gtk_version), '.')
    except:
	pygtk_version = '0.6.x'
	gtk_version = '1.2.x'

    banner = """Python %s, PyGTK %s (Gtk+ %s)
Interactive console to manipulate GTK+ widgets.""" % (python_version,
       pygtk_version,
       gtk_version)
    gi.interact(banner)
    
if __name__ == '__main__':
    interact()
