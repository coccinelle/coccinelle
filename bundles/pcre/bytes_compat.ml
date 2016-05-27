include String

external unsafe_to_string: t -> string = "%identity"

external unsafe_of_string: string -> t = "%identity"
