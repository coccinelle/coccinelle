# -*- coding: utf-8 -*- 

# Copyright (c) 2007 The PIDA Project

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


import os

# PIDA Imports
from pida.core.environment import get_data_path

from pida.ui.views import PidaView

from pida.utils.vim.vimembed import VimEmbedWidget
from pida.utils.vim.vimcom import VimCom

from pida.core.editors import EditorService, _


class VimView(PidaView):

    def create_ui(self):
        self._vim = VimEmbedWidget('gvim', self.svc.script_path)
        self.add_main_widget(self._vim)

    def run(self):
        return self._vim.run()

    def get_server_name(self):
        return self._vim.get_server_name()

    def grab_input_focus(self):
        self._vim.grab_input_focus()


class VimCallback(object):

    def __init__(self, svc):
        self.svc = svc

    def vim_new_serverlist(self, servers):
        if self.svc.server in servers:
            self.svc.init_vim_server()

    def vim_bufferchange(self, server, cwd, file_name, bufnum):
        if server == self.svc.server:
            if file_name:
                if os.path.abspath(file_name) != file_name:
                    file_name = os.path.join(cwd, file_name)
                if os.path.isdir(file_name):
                    self.svc.boss.cmd('filemanager', 'browse', new_path=file_name)
                    self.svc.boss.cmd('filemanager', 'present_view')
                    self.svc.open_last()
                else:
                    self.svc.boss.cmd('buffer', 'open_file', file_name=file_name)

    def vim_bufferunload(self, server, file_name):
        if server == self.svc.server:
            if file_name:
                self.svc.remove_file(file_name)
                self.svc.boss.get_service('buffer').cmd('close_file', file_name=file_name)

    def vim_filesave(self, server, file_name):
        if server == self.svc.server:
            self.svc.boss.cmd('buffer', 'current_file_saved')

    def vim_cursor_move(self, server, line_number):
        if server == self.svc.server:
            self.svc.set_current_line(int(line_number))

    def vim_shutdown(self, server, args):
        if server == self.svc.server:
            self.svc.boss.stop(force=True)

    def vim_complete(self, server, temp_buffer_filename, offset):
        buffer = open(temp_buffer_filename).read()
        offset = int(offset) - 1
        from rope.ide.codeassist import PythonCodeAssist
        from rope.base.project import Project
        p = Project(self.svc.boss.cmd('buffer', 'get_current').directory)
        c = PythonCodeAssist(p)
        co = c.assist(buffer, offset).completions
        print(co)
        for comp in co:
            self.svc._com.add_completion(server, comp.name)
        # do this a few times
        #self.svc._com.add_completion(server, 'banana')
        pass


# Service class
class Vim(EditorService):
    """Describe your Service Here""" 

    ##### Vim Things

    def _create_initscript(self):
        self.script_path = get_data_path('pida_vim_init.vim')

    def init_vim_server(self):
        if self.started == False:
            self._com.stop_fetching_serverlist()
            self.started = True
            self._emit_editor_started()

    def _emit_editor_started(self):
        self.boss.get_service('editor').emit('started')

    def get_server_name(self):
        return self._view.get_server_name()

    server = property(get_server_name)

    def pre_start(self):
        """Start the editor"""
        self.started = False
        self._create_initscript()
        self._cb = VimCallback(self)
        self._com = VimCom(self._cb)
        self._view = VimView(self)
        self.boss.cmd('window', 'add_view', paned='Editor', view=self._view)
        self._documents = {}
        self._current = None
        self._sign_index = 0
        self._signs = {}
        self._current_line = 1
        success = self._view.run()
        if not success:
            err = _('There was a problem running the "gvim" '
                             'executable. This is usually because it is not '
                             'installed. Please check that you can run "gvim" '
                             'from the command line.')
            self.error_dlg(err)
            raise RuntimeError(err)


    def open(self, document):
        """Open a document"""
        if document is not self._current:
            if document.unique_id in self._documents:
                fn = document.filename
                self._com.change_buffer(self.server, fn)
                self._com.foreground(self.server)
            else:
                self._com.open_file(self.server, document.filename)
                self._documents[document.unique_id] = document
            self._current = document


    def open_many(documents):
        """Open a few documents"""

    def open_last(self):
        self._com.change_buffer(self.server, '#')

    def close(self, document):
        if document.unique_id in self._documents:
            self._remove_document(document)
            self._com.close_buffer(self.server, document.filename)

    def remove_file(self, file_name):
        document = self._get_document_for_filename(file_name)
        if document is not None:
            self._remove_document(document)

    def _remove_document(self, document):
        del self._documents[document.unique_id]

    def _get_document_for_filename(self, file_name):
        for uid, doc in self._documents.iteritems():
            if doc.filename == file_name:
                return doc

    def close_all():
        """Close all the documents"""

    def save(self):
        """Save the current document"""
        self._com.save(self.server)

    def save_as(filename):
        """Save the current document as another filename"""

    def revert():
        """Revert to the loaded version of the file"""

    def goto_line(self, line):
        """Goto a line"""
        self._com.goto_line(self.server, line)
        self.grab_focus()

    def cut(self):
        """Cut to the clipboard"""
        self._com.cut(self.server)

    def copy(self):
        """Copy to the clipboard"""
        self._com.copy(self.server)

    def paste(self):
        """Paste from the clipboard"""
        self._com.paste(self.server)

    def undo(self):
        self._com.undo(self.server)

    def redo(self):
        self._com.redo(self.server)

    def grab_focus(self):
        """Grab the focus"""
        self._view.grab_input_focus()

    def define_sign_type(self, name, icon, linehl, text, texthl):
        self._com.define_sign(self.server, name, icon, linehl, text, texthl)

    def undefine_sign_type(self, name):
        self._com.undefine_sign(self.server, name)

    def _add_sign(self, type, filename, line):
        self._sign_index += 1
        self._signs[(filename, line, type)] = self._sign_index
        return self._sign_index
        
    def _del_sign(self, type, filename, line):
            return self._signs.pop((filename, line, type))

    def show_sign(self, type, filename, line):
        index = self._add_sign(type, filename, line)
        self._com.show_sign(self.server, index, type, filename, line)
   
    def hide_sign(self, type, filename, line):
        try:
            index = self._del_sign(type, filename, line)
            self._com.hide_sign(self.server, index, filename)
        except KeyError:
            self.window.error_dlg(_('Tried to remove non-existent sign'))
   
    def set_current_line(self, line_number):
        self._current_line = line_number

    def get_current_line(self):
        return self._current_line

    def delete_current_word(self):
        self._com.delete_cword(self.server)

    def insert_text(self, text):
        self._com.insert_text(self.server, text)

    def call_with_current_word(self, callback):
        return self._com.get_cword(self.server, callback)

    def call_with_selection(self, callback):
        return self._com.get_selection(self.server, callback)

    def set_path(self, path):
        return self._com.set_path(self.server, path)

# Required Service attribute for service loading
Service = Vim



# vim:set shiftwidth=4 tabstop=4 expandtab textwidth=79:
