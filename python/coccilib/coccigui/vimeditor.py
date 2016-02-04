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

import os
import gobject

import pida.core.service as service
from pida.core import errors

defs = service.definitions
types = service.types

import vimcom

class vim_editor(object):

    single_view = None

    class display(defs.optiongroup):
        class colour_scheme(defs.option):
            """The colour scheme to use in vim (Empty will be ignored)."""
            rtype = types.string
            default = ''
        class hide_vim_menu(defs.option):
            """Whether the vim menu will be hidden."""
            rtype = types.boolean
            default = False

    def init(self):
        self.__servers = {}
        self.__documents = {}
        self.__old_shortcuts = {'n':{}, 'v':{}}
        self.__currentdocument = None
        self._create_initscript()
        self.__cw = vimcom.communication_window(self)
        self.__newdocs = {}

    def _create_initscript(self):
        script_path = os.path.join(self.boss.get_pida_home(), 'pida_vim_init.vim')
        if not os.path.exists(script_path):
            f = open(script_path, 'w')
            f.write(vimcom.VIMSCRIPT)
            f.close()

    def vim_init_server(self):
        self.__cw.load_script(self.server,
            os.path.join(self.boss.get_pida_home(), 'pida_vim_init.vim'))

    def stop_fetching_serverlist(self):
        self.__cw.keep_fetching_serverlist = False

    def get_server(self):
        raise NotImplementedError

    def vim_start(self):
        raise NotImplementedError

    def vim_new_serverlist(self, serverlist):
        raise NotImplementedError

    def cmd_start(self):
        self.vim_start()

    def cmd_revert(self):
        self.__cw.revert(self.server)

    def cmd_close(self, document):
        if document.unique_id in self.__newdocs:
            fn = self.__newdocs[document.unique_id]
        else:
            fn = document.filename
        self.__cw.close_buffer(self.server, fn)

    def cmd_edit(self, document):
        """Open and edit."""
        if document is not self.__currentdocument:
            if (document.unique_id in
                self.__servers.setdefault(self.server, [])):
                if document.unique_id in self.__newdocs:
                    fn = self.__newdocs[document.unique_id]
                else:
                    fn = document.filename
                self.__cw.change_buffer(self.server, fn)
                self.__cw.foreground(self.server)
            else:
                found = False
                for server in self.__servers:
                    serverdocs = self.__servers[server]
                    if document.unique_id in serverdocs:
                        self.__cw.change_buffer(server, document.filename)
                        self.__cw.foreground(server)
                        found = True
                        break
                if not found:
                    if document.filename is None:
                        newname = self.__cw.new_file(self.server)
                        self.__newdocs[document.unique_id] = newname
                    else:
                        self.__cw.open_file(self.server, document.filename)
                    self.__servers[self.server].append(document.unique_id)
                    self.__documents[document.unique_id] = document
            self.__currentdocument = document
        if self.single_view is not None:
            self.single_view.raise_page()
            if document.filename is None:
                title = 'New File'
            else:
                title = document.filename
            self.single_view.long_title = title

    def cmd_undo(self):
        self.__cw.undo(self.server)

    def cmd_redo(self):
        self.__cw.redo(self.server)

    def cmd_cut(self):
        self.__cw.cut(self.server)

    def cmd_copy(self):
        self.__cw.copy(self.server)

    def cmd_paste(self):
        self.__cw.paste(self.server)

    def cmd_save(self):
        self.__cw.save(self.server)

    def cmd_save_as(self, filename):
        del self.__newdocs[self.__currentdocument.unique_id]
        self.__cw.save_as(self.server, filename)

    def cmd_goto_line(self, linenumber):
        self.__cw.goto_line(self.server, linenumber + 1)

    def cmd_show_mark(self, index, filename, line):
        self.__cw.show_sign(self.server, index, filename, line)
    
    def cmd_hide_mark(self, index):
        pass

    def reset(self):
        colorscheme = self.opts.display__colour_scheme
        if colorscheme:
            self.__cw.set_colorscheme(self.server, colorscheme)
        if self.opts.display__hide_vim_menu:
            self.__cw.set_menu_visible(self.server, False)
            #self.__load_shortcuts()

    def open_file_line(self, filename, linenumber):
        if self.__currentfile != filename:
            self.open_file(filename)
            self.__bufferevents.append([self.goto_line, (linenumber, )])
        else:
            self.goto_line(linenumber)

    def goto_line(self, linenumber):
        self.__cw.change_cursor(self.server, 1, linenumber)

    def vim_bufferchange(self, server, cwd, filename, bufnr):
        self.log.debug('vim buffer change "%s"', filename)
        if not filename or filename in '-MiniBufExplorer-':
            return
        if os.path.abspath(filename) != filename:
            filename = os.path.join(cwd, filename)
        if os.path.isdir(filename):
            if self.opts.behaviour__open_directories_in_pida:
                self.boss.call_command('filemanager', 'browse',
                    directory=filename)
                self.__cw.close_buffer(self.server, filename)
            return
        if self.__currentdocument is None or filename != self.__currentdocument.filename:
            for uid, fn in self.__newdocs.iteritems():
                if fn == filename:
                    doc = self.__documents[uid]
                    self.__current_doc_set(doc)
                    return
            for doc in self.__documents.values():
                if doc.filename == filename:
                    self.__current_doc_set(doc)
                    return
            self.boss.call_command('buffermanager', 'open_file',
                                    filename=filename)

    def __current_doc_set(self, doc):
        self.__currentdocument = doc
        self.boss.call_command('buffermanager', 'open_document',
                                document=doc)

    def vim_bufferunload(self, server, filename, *args):
        self.log.debug('vim unloaded "%s"', filename)
        if filename != '':
            doc = None
            for uid, fn in self.__newdocs.iteritems():
                if fn == filename:
                    doc = self.__documents[uid]
                    break
            if doc is None:
                for uid, document in self.__documents.iteritems():
                    if document.filename == filename:
                        doc = document
                        break
            if doc is not None:
                self.__servers[server].remove(doc.unique_id)
                del self.__documents[uid]
                self.__currentdocument = None
                self.boss.call_command('buffermanager', 'document_closed',
                                            document=doc, dorefresh=True)

    def vim_started(self, server):
        print('started')

    def vim_filesave(self, server, *args):
        self.boss.call_command('buffermanager', 'reset_current_document')

    def vim_globalkp(self, server, name):
        self.boss.command('keyboardshortcuts', 'keypress-by-name',
                           kpname=name)

    def vim_shutdown(self, server, *args):
        #self.clean_after_shutdown(server)
        self.after_shutdown(server)

    def vim_set_breakpoint(self, server, line):
        self.boss.call_command('pythondebugger', 'set_breakpoint',
            filename=self.__currentdocument.filename, line=int(line))

    def clean_after_shutdown(self, server):
        for docid in self.__servers.setdefault(server, []):
            doc = self.__documents[docid]
            del self.__documents[docid]
            self.boss.call_command('buffermanager', 'document_closed',
                                    document=doc)
        self.__servers[server] = []
        self.__currentdocument = None

    def after_shutdown(self, server):
        pass

    def get_vim_window(self):
        return self.__cw
    vim_window= property(get_vim_window)

    def get_current_document(self):
        return self.__currentdocument
    current_document = property(get_current_document)

    def stop(self):
        self.__cw.quit(self.server)
