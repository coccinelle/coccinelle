from coccinelle import Cocci

class Iteration:
    def __init__(self):
        self.__files = None
        self.__virtual_rules = set()
        self.__virtual_identifiers = {}
        self.__extend_virtual_ids = False

    def set_files(self, files):
        self.__files = files

    def add_virtual_rule(self, rule):
        self.__virtual_rules.add(rule)

    def add_virtual_identifier(self, ident, value):
        if ident in self.__virtual_identifiers:
            raise ValueError("multiple values specified for {}".format(ident))
        self.__virtual_identifiers[ident] = value

    def set_extend_virtual_ids(self, value):
        self.__extend_virtual_ids(value)

    def register(self):
        Cocci().add_pending_instance(
            self.__files, list(self.__virtual_rules),
            list(self.__virtual_identifiers.items()), self.__extend_virtual_ids)
