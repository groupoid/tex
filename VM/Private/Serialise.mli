
open Vm_types.Types

val serialise_unknown : Tools.IO.ostream -> unknown -> unit
val unserialise_unknown : Tools.IO.irstream -> partial_value

