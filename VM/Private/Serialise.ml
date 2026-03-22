
open Vm_types.Types

let serialise_unknown _os _v =
  runtime_error "Serialisation not implemented"

let unserialise_unknown _is =
  runtime_error "Unserialisation not implemented"