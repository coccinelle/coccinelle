# -*- coding: utf-8 -*- 

# vim:set shiftwidth=4 tabstop=4 expandtab textwidth=79:
#Copyright (c) 2005-2006 The PIDA Project

#Permission is hereby granted, free of charge, to any person obtaining a copy
#of this software and associated documentation files (the "Software"), to deal
#in the Software without restriction, including without limitation the rights
#to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#copies of the Software, and to permit persons to whom the Software is
#furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in
#all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#SOFTWARE.


''' A library to embed vim in a gtk socket '''


import gtk
import os
import time


import subprocess

class vim_embed(object):

    HAS_CONTROL_BOX = False
    
    HAS_TITLE = False

    def init(self, command='gvim', args=[]):
        self.__servername = self.__generate_servername()
        self.pid = None
        self.args = args
        self.r_cb_plugged = None
        self.r_cb_unplugged = None
        self.__eb = None

    def __pack(self):
        socket = gtk.Socket()
        eb = gtk.EventBox()
        self.widget.pack_start(eb)
        eb.add_events(gtk.gdk.KEY_PRESS_MASK)
        eb.add(socket)
        self.show_all()
        self.__eb = eb
        return socket.get_id()

    def __generate_servername(self):
        return 'PIDA_EMBEDDED_%s' % time.time()

    def get_servername(self):
        return self.__servername
    servername = property(get_servername)

    def should_remove(self):
        self.service.remove_attempt()
        return False

    def run(self, command):
        self.command = command
        xid = self.__pack()
        args = self.args[:] # a copy
        args.extend(['--socketid', '%s' % xid])
        if not xid:
            return
        if not self.pid:
            popen = subprocess.Popen([self.command, '--servername',
                                      self.servername, '--cmd',
                                      'let PIDA_EMBEDDED=1'] + args,
                                      close_fds=True)
            self.pid = popen.pid
        self.show_all()
        
    def grab_input_focus(self):
        self.__eb.child_focus(gtk.DIR_TAB_FORWARD)

class VimEmbedWidget(gtk.EventBox):

    def __init__(self, command, script_path, args=[]):
        gtk.EventBox.__init__(self)
        self._servername = self._generate_servername()
        self._command = command
        self._init_script = script_path
        self.pid = None
        self.args = args
        self.r_cb_plugged = None
        self.r_cb_unplugged = None
        self.__eb = None

    def _create_ui(self):
        socket = gtk.Socket()
        self.add_events(gtk.gdk.KEY_PRESS_MASK)
        self.add(socket)
        self.show_all()
        return socket.get_id()

    def _generate_servername(self):
        return 'PIDA_EMBEDDED_%s' % time.time()

    def get_server_name(self):
        return self._servername

    def should_remove(self):
        self.service.remove_attempt()
        return False

    def run(self):
        xid = self._create_ui()
        args = self.args[:] # a copy
        args.extend(['--socketid', '%s' % xid])
        if not xid:
            return
        if not self.pid:
            try:
                popen = subprocess.Popen(
                    [self._command,
                    '--servername', self.get_server_name(),
                    '--cmd', 'let PIDA_EMBEDDED=1',
                    '-c', 'so %s' % self._init_script
                    ] + args,
                    close_fds=True
                )
                self.pid = popen.pid
            except OSError:
                return False
        self.show_all()
        return True

    def grab_input_focus(self):
        self.child_focus(gtk.DIR_TAB_FORWARD)

