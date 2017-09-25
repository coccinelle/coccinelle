class Location:
 def __init__(self, file, current_element, ce_line, ce_column, ce_line_end, ce_column_end, line, column, line_end, column_end):
                self.file = file
                self.current_element = current_element
                self.current_element_line = ce_line
                self.current_element_column = ce_column
                self.current_element_line_end = ce_line_end
                self.current_element_column_end = ce_column_end
                self.line = line
                self.column = column
                self.line_end = line_end
                self.column_end = column_end

class ElemBase:
        def __init__(self):
                pass

# class Expression(ElemBase):
#       def __init__(self, expr):
#               ElemBase.__init__(self)
#               self.expr = expr
#
#       def __str__(self):
#               return self.expr

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
#       def __init__(self, ident):
#               ElemBase.__init__(self)
#               self.ident = ident
#
#       def __str__(self):
#               return self.ident
