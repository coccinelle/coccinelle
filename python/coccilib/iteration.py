from coccinelle import Cocci
from sys import version_info

# from "six" library and
# http://stackoverflow.com/questions/4843173/how-to-check-if-type-of-a-variable-is-string-in-python
PY3 = version_info[0] == 3

if PY3:
    string_types = str
else:
    string_types = basestring

def string_check(value):
    return isinstance(value, string_types)

class Iteration:
    def __init__(self):
        self.__files = None
        self.__virtual_rules = set()
        self.__virtual_identifiers = {}
        self.__extend_virtual_ids = False

    def set_files(self, files):
        if not (isinstance(files, list)
                and all(string_check(value) for value in files)):
            raise TypeError("Iteration.set_files expects a list of strings")
        self.__files = files

    def add_virtual_rule(self, rule):
        if not (string_check(rule)):
            raise TypeError("Iteration.add_virtual_rule expects a string")
        self.__virtual_rules.add(rule)

    def add_virtual_identifier(self, ident, value):
        if not (string_check(ident) and string_check(value)):
            raise TypeError(
                "Iteration.add_virtual_identifier expects string arguments")
        if ident in self.__virtual_identifiers:
            raise ValueError("multiple values specified for {}".format(ident))
        self.__virtual_identifiers[ident] = value

    def set_extend_virtual_ids(self, value):
        if not (string_check(value)):
            raise TypeError("Iteration.set_extend_virtual_ids expects a string")
        self.__extend_virtual_ids(value)

    def register(self):
        Cocci().add_pending_instance(
            self.__files, list(self.__virtual_rules),
            list(self.__virtual_identifiers.items()), self.__extend_virtual_ids)
