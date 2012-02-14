class Location:
 def __init__(self, file, current_element, line, column, line_end, column_end):
		self.file = file
		self.current_element = current_element
		self.line = line
		self.column = column
		self.line_end = line_end
		self.column_end = column_end

class ElemBase:
	def __init__(self):
		pass

# class Expression(ElemBase):
# 	def __init__(self, expr):
# 		ElemBase.__init__(self)
# 		self.expr = expr
#
# 	def __str__(self):
# 		return self.expr

class TermList(ElemBase):
	def __init__(self, expr, elements):
		ElemBase.__init__(self)
		self.expr = expr
		self.elements = elements

	def __getitem__(self,n):
		return self.elements[n]

	def __str__(self):
		return self.expr

# class Identifier(ElemBase):
# 	def __init__(self, ident):
# 		ElemBase.__init__(self)
# 		self.ident = ident
#
# 	def __str__(self):
# 		return self.ident
