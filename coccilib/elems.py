class Location:
	def __init__(self, file, line, column):
		self.file = file
		self.line = line
		self.column = column

class ElemBase:
	def __init__(self, loc):
		self.location = loc

class Expression(ElemBase):
	def __init__(self, expr, loc):
		ElemBase.__init__(self, loc)
		self.expr = expr

	def __str__(self):
		return self.expr

class Identifier(ElemBase):
	def __init__(self, ident, loc):
		ElemBase.__init__(self, loc)
		self.ident = ident

	def __str__(self):
		return self.ident
