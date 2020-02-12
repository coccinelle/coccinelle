from copy import deepcopy

class Output:
        """In order to implement an output class for use with Coccinelle,
        one can inherit from this class and overload register_match with
        the same number of arguments.

        include_match will be overwritten by inheriting from your actual
        class, and thus if your class is a.b.C then Coccinelle will create
        a Python class "class Coccinelle(a.b.C)" that hooks include_match
        into the O'Caml internals.
        """
        def include_match(self, b):
                pass

        def register_match(self, include, messages):
                pass

        def combine(self, meta_variable, locations):
                nmv = deepcopy(meta_variable)
                nloc = [deepcopy(loc) for loc in locations]
                nmv.location = nloc[0]
                nmv.locations = nloc

                return nmv

        def finalise(self):
                pass

        def print_main (self, *args):
                from coccilib.org import print_main
                print_main(*args)

        def print_sec (self, *args):
                from coccilib.org import print_sec
                print_sec(*args)

        def print_secs (self, *args):
                from coccilib.org import print_secs
                print_secs(*args)

class Console(Output):
        def __init__(self):
                pass

        def register_match(self, include, messages):
                self.include_match(include)
                if include:
                        for variable, message in messages:
                                print("%s:%s:%s: %s - %s" % (variable.location.file, variable.location.line, variable.location.column, message, variable))
