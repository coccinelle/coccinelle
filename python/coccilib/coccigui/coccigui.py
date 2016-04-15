import pygtk
pygtk.require("2.0")
import sys, gtk, gtk.glade, gobject, os, locale, gettext
from pycoccimodel import *
import vimembed, vimcom

class VimCallback(object):
  def __init__(self, app):
    self.app = app

  def vim_new_serverlist(self, serverlist):
    self.app.vimcom.stop_fetching_serverlist()
    self.app.vimcom.open_file(self.app.vimsock.get_server_name(), '~/.vimrc')

class pycocci(object):
  def __init__(self):
    self.local_path = os.path.realpath(os.path.dirname(sys.argv[0]))
    self.initialize_translation()
    self.gladefile = os.path.join(self.local_path, "coccilib/coccigui/pygui.glade")
    self.wTree = gtk.glade.XML(self.gladefile, "mainWindow")
    self.setup_tree_columns()
    self.wTree.signal_autoconnect(self)
    self.initialise_tree()
    self.main_window = self.wTree.get_widget("mainWindow")

    self.vimsock = vimembed.VimEmbedWidget('gvim', '~/.vimrc')
    self.vimsock.visible = True
    self.vbox1 = self.wTree.get_widget("vbox1")
    self.vbox1.pack_end(self.vimsock)
    self.setup_vim()

  def setup_vim(self):
    self.vimcb = VimCallback(self)
    self.vimcom = vimcom.VimCom(self.vimcb)
    self.vimsock.run()
    self.vimsock.connect('destroy', self.on_vimsock_destroy)

  def on_vimsock_destroy(self, widget):
    self.setup_vim()

  def initialise_tree(self):
    tree_type_list = []
    self.__column_dict = {}
    self.bugTreeView = self.wTree.get_widget("bugView")
    self.bugTreeView.set_rules_hint(True)

    for c in self.__tree_columns:
      self.__column_dict[c.id] = c
      tree_type_list.append(c.type)

      if c.visible:
        column = gtk.TreeViewColumn(c.name, c.cellrenderer, text=c.pos)
        column.set_resizable(True)
        column.set_sort_column_id(c.pos)
        self.bugTreeView.append_column(column)

    self.bugTree = gtk.TreeStore(*tree_type_list)
    self.bugTreeView.set_model(self.bugTree)
    self.bugTreeView.connect('row-activated', self.row_activated)

  def row_activated(self, view, path, view_column):
          it = view.get_model().get_iter(path)
          obj, objtype, bugdesc, file, line, col = view.get_model().get(it, 0, 1, 2, 3, 4, 5)

          if file != '':
                  server = self.vimsock.get_server_name()

                  self.vimcom.open_file(server, file)
                  if line != '':
                          self.vimcom.send_ex(server, line)

                          if col != '':
                                  self.vimcom.send_keys(server, col + '|')

  def setup_tree_columns(self):
    self.__tree_columns = [
      pycoccicolumn(COL_OBJECT, gobject.TYPE_PYOBJECT, "object", 0),
      pycoccicolumn(COL_OBJECT, gobject.TYPE_INT, "object_type", 1),
      pycoccicolumn(COL_TITLE, gobject.TYPE_STRING, _("Bug type"), 2, True, gtk.CellRendererText()),
      pycoccicolumn(COL_FILE, gobject.TYPE_STRING, _("File"), 3, True, gtk.CellRendererText()),
      pycoccicolumn(COL_LINE, gobject.TYPE_STRING, _("Line"), 4, True, gtk.CellRendererText()),
      pycoccicolumn(COL_COLUMN, gobject.TYPE_STRING, _("Column"), 5, True, gtk.CellRendererText())
      ]

  def initialize_translation(self):
    langs = []
    lc, encoding = locale.getdefaultlocale()
    if lc:
      langs = [lc]
    language = os.environ.get('LANGUAGE', None)
    if language:
      langs += language.split(':')
    
    gettext.bindtextdomain('pycocci', self.local_path)
    gettext.textdomain('pycocci')
    self.lang = gettext.translation('pycocci', self.local_path,
      languages=langs, fallback = True)
    gettext.install('pycocci', self.local_path)

  def on_mainWindow_destroy(self, widget):
    gtk.main_quit()

  def add_result(self, cocci_file, l):
    root = self.bugTreeView.get_model().get_iter_root()
    it = None

    while root != None:
      c_cocci_file = self.bugTreeView.get_model().get(root, 2)[0]
      if c_cocci_file == cocci_file:
        it = root
        break

      root = self.bugTreeView.get_model().iter_next(root)

    if it == None:
      it = self.bugTree.insert_after(None, None, (None, 0, cocci_file, '', '', ''))

    description, file, line, col = l[0]
    mit = self.bugTree.append(it, (None, 1, description, file, line, col))

    for i in xrange(1, len(l)):
      description, file, line, col = l[i]
      self.bugTree.append(mit, (None, 2, description, file, line, col))

#if __name__ == '__main__':
#  app = pycocci()
#  app.add_result('Test.cocci', [('Array identified: z', '/home/hstuart/thesis/py-cocci/tests/scripting/array/script4.c', '6', '7'), ('Array use: z', '/home/hstuart/thesis/py-cocci/tests/scripting/array/script4.c', '8', '3')])
#  app.add_result('Test.cocci', [('Array identified: foo', '/home/hstuart/thesis/py-cocci/tests/scripting/array/script4.c', '12', '17')])
#  gtk.main()
