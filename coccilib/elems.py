class Location:
	def __init__(self, file, line, column, line_end, column_end):
		self.file = file
		self.line = line
		self.column = column
		self.line_end = line_end
		self.column_end = column_end

class ElemBase:
	def __init__(self):
		pass


class Expression(ElemBase):
	def __init__(self, expr):
		ElemBase.__init__(self)
		self.expr = expr

	def __str__(self):
		return self.expr

class Identifier(ElemBase):
	def __init__(self, ident):
		ElemBase.__init__(self)
		self.ident = ident

	def __str__(self):
		return self.ident
