import pygtk
import gtk, gtk.glade

COL_OBJECT = 0
COL_OBJECT_TYPE = 1
COL_TITLE = 2
COL_FILE = 3
COL_LINE = 4
COL_COLUMN = 5

class pycoccicolumn(object):
  def __init__(self, id, type, name, pos, visible=False, cellrenderer=None):
    self.id = id
    self.type = type
    self.name = name
    self.pos = pos
    self.visible = visible
    self.cellrenderer = cellrenderer
    self.colour = 0

  def __str__(self):
    return "<pycoccicolumn object: ID = %s type = %s name = %s pos = %d visible = %s cellrenderer = %s>" % (self.id, self.type, self.name, self.pos, self.visible, self.cellrenderer)
