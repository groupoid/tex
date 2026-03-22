
type uc_char = int
type uc_list = uc_char list
type uc_string = uc_char array

module DynamicCharMap : (Map.S with type key = uc_char)

